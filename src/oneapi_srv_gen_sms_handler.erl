-module(oneapi_srv_gen_sms_handler).

-behaviour(cowboy_http_handler).

%% cowboy_http_handler callbacks
-export([
    init/3,
    handle/2,
    terminate/3
]).

%% cowboy hooks
-export([
    onrequest_hook/1,
    onresponse_hook/4
]).

-include("application.hrl").
-include("oneapi_srv.hrl").
-include_lib("alley_common/include/logging.hrl").

-record(state, {
    req         :: term(),
    mod         :: atom(),
    mstate      :: term(),
    creds       :: credentials(),
    sender_addr :: binary(),
    thendo      :: fun(),
    thendo_args :: term()
}).

-define(E_CREDIT_LIMIT_EXCEEDED, <<"Customer's postpaid credit limit is exceeded">>).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================

-callback init(credentials()) ->
    {ok, state()} |
    {error, term()}.

-callback handle_send_outbound(outbound_sms(), state()) ->
    {ok, request_id()} |
    {error, term()}.

-callback handle_query_delivery_status(sender_address(), request_id(), state()) ->
    {ok, sms_delivery_statuses()}  |
    {error, term()}.

-callback handle_subscribe_to_delivery_notifications(subscribe_delivery_notifications(), state()) ->
    {ok, subscription_id()} |
    {error, term()}.

-callback handle_unsubscribe_from_delivery_notifications(subscription_id(), state()) ->
    {ok, deleted} |
    {error, term()}.

-callback handle_retrieve_inbound(retrieve_sms_req(), state()) ->
    {ok, [inbound_sms()], pending_sms()} |
    {error, term()}.

-callback handle_subscribe_to_inbound_notifications(subscribe_inbound(), state()) ->
    {ok, subscription_id()} |
    {error, term()}.

-callback handle_unsubscribe_from_inbound_notifications(subscription_id(), state()) ->
    {ok, deleted} |
    {error, term()}.

%% ===================================================================
%% cowboy_http_hander callbacks
%% ===================================================================

init({_Any, http}, Req, [Module]) ->
    {ok, Req, #state{mod = Module, req = Req}}.

handle(Req, State = #state{}) ->
    {Path, Req} = cowboy_req:path(Req),
    {Method, Req} = cowboy_req:method(Req),
    case get_credentials(Req) of
        {ok, {CustomerId, UserId, Password}} ->
            Creds = #credentials{
                customer_id = CustomerId,
                user_id = UserId,
                password = Password
            },
            [<<>> | Segments] = binary:split(Path, <<"/">>, [global]),
            handle_req(Method, Segments, State#state{creds = Creds});
        {error, authentication} ->
            oneapi_srv_protocol:code(401, Req, [])
    end.

terminate(_Reason, _Req, _State) ->
    clean_body(),       %% Need to cleanup body record in proc dict
    ok.                 %% since cowboy uses one process per several
                        %% requests in keepalive mode

%% ===================================================================
%% Parsing http requests
%% ===================================================================

handle_req(<<"POST">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _RawSenderAddr, <<"requests">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_send_outbound(Args, St) end,
    Args = [],
    do_init(State#state{
        req = Req2,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"GET">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _RawSenderAddr, <<"requests">>, _ReqId, <<"deliveryInfos">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    {ReqId, Req3} = cowboy_req:binding(request_id, Req2),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_query_delivery_status(Args, St) end,
    Args = ReqId,
    do_init(State#state{
        req = Req3,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"POST">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _RawSenderAddr, <<"subscriptions">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_subscribe_to_delivery_notifications(Args, St) end,
    Args = [],
    do_init(State#state{
        req = Req2,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

%% This handler doesn't conform to OneAPI v2 (http://www.gsma.com/oneapi/sms-restful-api/).
%% According to the specification there must not be the Sender Address.
%% But OneAPI v3 (http://www.gsma.com/oneapi/sms-restful-netapi/) has the Sender Address.
handle_req(<<"DELETE">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _RawSenderAddr, <<"subscriptions">>, _SubId],
    State = #state{req = Req, creds = Creds}
) ->
    {SubId, Req2} = cowboy_req:binding(subscription_id, Req),
    AfterInit = fun(Args, St) -> process_unsubscribe_from_delivery_notifications(Args, St) end,
    Args = SubId,
    do_init(State#state{
        req = Req2,
        sender_addr = undefined,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"DELETE">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, <<"subscriptions">>, _SubId],
    State = #state{req = Req, creds = Creds}
) ->
    {SubId, Req2} = cowboy_req:binding(subscription_id, Req),
    AfterInit = fun(Args, St) -> process_unsubscribe_from_delivery_notifications(Args, St) end,
    Args = SubId,
    do_init(State#state{
        req = Req2,
        sender_addr = undefined,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"GET">>,
    [_Ver, <<"smsmessaging">>, <<"inbound">>, <<"registrations">>, _RegId, <<"messages">>],
    State = #state{req = Req, creds = Creds}
) ->
    %% !!! registrationID agreed with the OneAPI operator !!!
    %% !!! We use Sender Address for this !!!
    {RegId, Req2} = cowboy_req:binding(registration_id, Req),
    AfterInit = fun(Args, St) -> process_retrieve_inbound(Args,St) end,
    Args = convert_addr(RegId),
    do_init(State#state{
        req = Req2,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"POST">>,
    [_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>],
    State = #state{creds = Creds}
) ->
    AfterInit = fun(Args, St) -> process_subscribe_to_inbound_notifications(Args, St) end,
    Args = [],
    do_init(State#state{
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"DELETE">>,
    [_Ver, <<"smsmessaging">>, <<"inbound">>, <<"subscriptions">>, _SubId],
    State = #state{req = Req, creds = Creds}
) ->
    {SubId, Req2} = cowboy_req:binding(subscription_id, Req),
    AfterInit = fun(Args, St) -> process_unsubscribe_from_inbound_notifications(Args, St) end,
    Args = SubId,
    do_init(State#state{
        req = Req2,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(_Method, _Segments, State = #state{req = Req}) ->
    oneapi_srv_protocol:code(404, Req, State).

%% ===================================================================
%% Handler initialization
%% ===================================================================

do_init(State = #state{
    thendo = Fun,
    thendo_args = Args,
    mod = Mod,
    req = Req,
    creds = Creds}
) ->
    case Mod:init(Creds) of
        {ok, MState} ->
            Fun(Args, State#state{mstate = MState});
        {error, authentication} ->
            oneapi_srv_protocol:code(401, Req, State);
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req, State)
    end.

%% ===================================================================
%% Send outbound
%% ===================================================================

process_send_outbound(_, State = #state{
    mstate = MState,
    mod = Mod,
    req = Req,
    sender_addr = _Addr
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    SendSmsReq = #outbound_sms{
        %% mandatory
        address        = gmv(QsVals, <<"address">>),
        message        = gv(QsVals, <<"message">>),
        sender_address = gv(QsVals, <<"senderAddress">>),
        %% optional
        client_correlator = gv(QsVals, <<"clientCorrelator">>),
        sender_name       = gv(QsVals, <<"senderName">>),
        notify_url        = gv(QsVals, <<"notifyURL">>),
        callback_data     = gv(QsVals, <<"callbackData">>)
    },
    case Mod:handle_send_outbound(SendSmsReq, MState) of
        {ok, ReqId} ->
            Location = build_resource_url(Req2, ReqId),
            Body = [
                {<<"resourceReference">>, [
                    {<<"resourceURL">>, Location}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [
                {<<"content-type">>, <<"application/json">>},
                {<<"location">>, Location}
            ],
            {ok, Req3} = cowboy_req:reply(201, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {error, originator_not_found} ->
            oneapi_srv_protocol:exception('svc0004', [<<"senderAddress">>], Req2, State);
        {error, no_recipients} ->
            oneapi_srv_protocol:exception('svc0004', [<<"address">>], Req2, State);
        {error, no_dest_addrs} ->
            oneapi_srv_protocol:exception('svc0004', [<<"address">>], Req2, State);
        {error, no_message_body} ->
            oneapi_srv_protocol:exception('svc0002', [<<"message">>], Req2, State);
        {error, credit_limit_exceeded} ->
            oneapi_srv_protocol:exception('svc0001', [?E_CREDIT_LIMIT_EXCEEDED], Req2, State);
        {error, receipts_not_allowed} ->
            oneapi_srv_protocol:exception('svc0283', [], Req2, State);
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req2, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req2, State)
    end.

%% ===================================================================
%% Query Delivery Status
%% ===================================================================

process_query_delivery_status(ReqId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req,
    sender_addr = SenderAddr
}) ->
    case Mod:handle_query_delivery_status(SenderAddr, ReqId, MState) of
        {ok, ResponseList} ->
            Reports = lists:map(
                fun({Address, Status})->
                    [{<<"address">>, Address}, {<<"deliveryStatus">>, Status}]
                end, ResponseList),
            Resource = build_resource_url(Req),
            Body = [
                {<<"deliveryInfoList">>, [
                    {<<"deliveryInfo">>, Reports},
                    {<<"resourceURL">>, Resource}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [{<<"content-type">>, <<"application/json">>}],
            {ok, Req2} = cowboy_req:reply(200, Headers, JsonBody, Req),
            {ok, Req2, State};
        {error, empty_request_id} ->
            oneapi_srv_protocol:exception('svc0002', [<<"requestId">>], Req, State);
        {error, invalid_request_id} ->
            oneapi_srv_protocol:exception('svc0002', [<<"requestId">>], Req, State);
        {error, receipts_not_allowed} ->
            oneapi_srv_protocol:exception('svc0283', [], Req, State);
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req, State)
    end.

%% ===================================================================
%% Subscribe to delivery notifications
%% ===================================================================

process_subscribe_to_delivery_notifications(_, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState,
    sender_addr = SenderAddr
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    Request = #subscribe_delivery_notifications{
        %% mandatory
        notify_url = gv(QsVals, <<"notifyURL">>),
        sender_addr = SenderAddr,
        %% optional
        client_correlator = gv(QsVals, <<"clientCorrelator">>),
        criteria          = gv(QsVals, <<"criteria">>),
        callback_data     = gv(QsVals, <<"callbackData">>)
    },
    case Mod:handle_subscribe_to_delivery_notifications(Request, MState) of
        {ok, SubId} ->
            CallBackData = gv(QsVals, <<"callbackData">>, <<>>),
            NotifyURL = gv(QsVals, <<"notifyURL">>, <<>>),
            Criteria = gv(QsVals, <<"criteria">>, <<>>),
            Location = build_resource_url(Req, SubId),
            Body = [
                {<<"deliveryReceiptSubscription">>, [
                    {<<"callbackReference">>, [
                        {<<"callbackData">>, CallBackData},
                        {<<"notifyURL">>, NotifyURL},
                        {<<"criteria">>, Criteria}
                    ]},
                    {<<"resourceURL">>, Location}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [
                {<<"content-type">>, <<"application/json">>},
                {<<"location">>, Location}
            ],
            {ok, Req3} = cowboy_req:reply(201, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {error, empty_notify_url} ->
            oneapi_srv_protocol:exception('svc0002', [<<"notifyURL">>], Req, State);
        {error, already_exists} ->
            Correlator = gv(QsVals, <<"clientCorrelator">>, <<>>),
            oneapi_srv_protocol:exception('svc0005', [Correlator, <<"clientCorrelator">>], Req, State);
        {error, receipts_not_allowed} ->
            oneapi_srv_protocol:exception('svc0283', [], Req2, State);
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req, State)
    end.

%% ===================================================================
%% Unsubscribe from delivery notifications
%% ===================================================================

process_unsubscribe_from_delivery_notifications(SubId, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState,
    sender_addr = undefined
}) ->
    case Mod:handle_unsubscribe_from_delivery_notifications(SubId, MState) of
        {ok, deleted} ->
            {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
            {ok, Req2, State};
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req, State)
    end.

%% ===================================================================
%% Retrieve outbound
%% ===================================================================

process_retrieve_inbound(RegId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    RetrieveSmsReq = #retrieve_sms_req{
        reg_id = RegId,
        batch_size = giv(QsVals, <<"maxBatchSize">>)
    },
    case Mod:handle_retrieve_inbound(RetrieveSmsReq, MState) of
        {ok, ListOfInboundSms, PendingSms} ->
            Messages = lists:map(
                fun(#inbound_sms{
                        date_time = DateTime,
                        message_id = MsgId,
                        message = Message,
                        sender_addr = SenderAddr
                    })->
                        ISO8601 = ac_datetime:datetime_to_iso8601(DateTime),
                        LocationUrl = build_resource_url(Req, MsgId),
                        [{<<"dateTime">>, ISO8601},
                         {<<"destinationAddress">>, RegId},
                         {<<"messageId">>, MsgId},
                         {<<"message">>, Message},
                         {<<"resourceURL">>, LocationUrl},
                         {<<"senderAddress">>, SenderAddr}]
                end, ListOfInboundSms),
            ThisBatchSize = length(ListOfInboundSms),
            ResourceURL = build_resource_url(Req),
            Body = [
                {<<"inboundSMSMessageList">>, [
                    {<<"inboundSMSMessage">>, Messages},
                    {<<"numberOfMessagesInThisBatch">>, ThisBatchSize},
                    {<<"resourceURL">>, ResourceURL},
                    {<<"totalNumberOfPendingMessages">>, PendingSms}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [{<<"content-type">>, <<"application/json">>}],
            {ok, Req3} = cowboy_req:reply(200, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {error, invalid_bax_match_size} ->
            oneapi_srv_protocol:exception('svc0002', [<<"maxBatchSize">>], Req, State);
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req2, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req2, State)
    end.

%% ===================================================================
%% Subscribe to inbound notifications
%% ===================================================================

process_subscribe_to_inbound_notifications( _, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    SubscribeInbound = #subscribe_inbound{
        dest_addr     = convert_addr(gv(QsVals, <<"destinationAddress">>, <<>>)),
        notify_url    = gv(QsVals, <<"notifyURL">>),
        criteria      = gv(QsVals, <<"criteria">>),
        callback_data = gv(QsVals, <<"callbackData">>),
        correlator    = gv(QsVals, <<"clientCorrelator">>)
    },
    case Mod:handle_subscribe_to_inbound_notifications(SubscribeInbound, MState) of
        {ok, SubId} ->
            Location = build_resource_url(Req, SubId),
            Body = [
                {<<"resourceReference">>, [
                    {<<"resourceURL">>, Location}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [
                {<<"content-type">>, <<"application/json">>},
                {<<"location">>, Location}
            ],
            {ok, Req3} = cowboy_req:reply(201, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {error, empty_dest_addr} ->
            oneapi_srv_protocol:exception('svc0002', [<<"destinationAddress">>], Req, State);
        {error, empty_notify_url} ->
            oneapi_srv_protocol:exception('svc0002', [<<"notifyURL">>], Req, State);
        {error, already_exists} ->
            Correlator = gv(QsVals, <<"clientCorrelator">>, <<>>),
            oneapi_srv_protocol:exception('svc0005', [Correlator, <<"clientCorrelator">>], Req, State);
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req, State)
    end.

%% ===================================================================
%% Unsubscribe from inbound notifications
%% ===================================================================

process_unsubscribe_from_inbound_notifications(SubId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req
}) ->
    case Mod:handle_unsubscribe_from_inbound_notifications(SubId, MState) of
        {ok, deleted} ->
            {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
            {ok, Req2, State};
        {error, timeout} ->
            oneapi_srv_protocol:code(503, Req, State);
        {error, Error} ->
            oneapi_srv_protocol:exception('svc0001', [Error], Req, State)
    end.

%% ===================================================================
%% Cowboy hooks
%% ===================================================================

%% Since app uses cowboy onresponse hook for logging purposes
%% and also need http request body in debug mode,
%% to overcome cowboy restriction about request body
%% (read http://ninenines.eu/docs/en/cowboy/HEAD/guide/req)
%% that the request body can only be done once, as it is
%% read directly from the socket, app uses following
%% strategy:
%% 1. Save request body to proc dictionary with onrequest hook
%% 2. Uses get_body() function to get body from dictionary
%% 3. Cleanup req_body with clean_body() function on terminate since
%% the same process can be used to process next request in kepepalive
%% mode (read http://ninenines.eu/docs/en/cowboy/HEAD/guide/internals
%% 'One process for many requests' section)

-spec onrequest_hook(cowboy_req:req()) -> cowboy_req:req().
onrequest_hook(Req) ->
    %% 2k recipients = 28k body for HTTP POST x-www-form-urlencoded
    {ok, Body, Req2} = cowboy_req:body(Req, [{length, 800000}]),
    put(req_body, Body),
    Req2.

-spec onresponse_hook(non_neg_integer(),
    list(), binary(), cowboy_req:req()) -> cowboy_req:req().
onresponse_hook(RespCode, RespHeaders, RespBody, Req) ->
    ReqBody = get_body(),
    alley_services_http_in_logger:log(
        RespCode, RespHeaders, RespBody, Req, ReqBody),
    {ok, Req2} =
        cowboy_req:reply(RespCode, RespHeaders, RespBody, Req),
    Req2.

get_body() ->
    get(req_body).

clean_body() ->
    put(req_body, undefined).

%% ===================================================================
%% Internal
%% ===================================================================

get_qs_vals(Req) ->
    {Method, Req2} = cowboy_req:method(Req),
    {QsVals2, Req4} =
        case Method of
            <<"POST">> ->
                BodyQs = cow_qs:parse_qs(get_body()),
                {BodyQs, Req2};
            _Any ->
                {QsVals, Req3} = cowboy_req:qs_vals(Req2),
                {QsVals, Req3}
        end,
    {QsVals2, Req4}.

convert_addr(<<"tel:+", Bin/binary>>) ->
    Bin;
convert_addr(<<"tel:", Bin/binary>>) ->
    Bin;
convert_addr(Bin) when is_binary(Bin) ->
    Bin.

%% Return integer value from proplist of request
giv(QsVals, Key) ->
    case gv(QsVals, Key) of
        undefined ->
            undefined;
        Value ->
            try binary_to_integer(Value)
            catch _:_ -> -1
            end
    end.

gv(QsVals, Key, Default) ->
    case gv(QsVals, Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.

gv(QsVals, Key) ->
    case lists:keytake(Key, 1, QsVals) of
        {value, {_, Value}, _TupleList2} ->
            Value;
        _ ->
            undefined
    end.

gmv(QsVals, Key) ->
    [V || {K, V} <- QsVals, K =:= Key].

build_resource_url(Req) ->
    build_resource_url(Req, <<>>).
build_resource_url(Req, ItemId) when is_binary(ItemId) ->
    {Host, _} = cowboy_req:host(Req),
    {Path, _} = cowboy_req:path(Req),
    {PortI, _} = cowboy_req:port(Req),
    Port = integer_to_binary(PortI),
    Protocol = <<"http://">>,
    ReqId = case ItemId of
                <<>> -> <<>>;
                Any  -> << <<"/">>/binary, Any/binary>>
            end,
    <<Protocol/binary,
      Host/binary, <<":">>/binary, Port/binary,
      Path/binary, ReqId/binary>>.

%% ===================================================================
%% Credentials
%% ===================================================================

get_credentials(Req) ->
    {Header, Req} = cowboy_req:header(<<"authorization">>, Req),
    case application:get_env(?APP, customer_user_delimiter) of
        {ok, Delimiter} ->
            parse_credential_header(Header, [Delimiter]);
        undefined ->
            parse_credential_header(Header, [])
    end.

parse_credential_header(undefined, _Delimiter) ->
    {error, authentication};
parse_credential_header(Header, Delimiter) ->
    List = binary:split(Header, [<<"Basic">>, <<" ">>], [global]),
    [Base64] = [Item || Item <- List, Item =/= <<>>],
    Creds = base64:decode(Base64),
    case binary:split(Creds, [<<":">>] ++ Delimiter, [global]) of
        [CustomerId, UserId, Password] ->
            {ok, {CustomerId, UserId, Password}};
        _ ->
            ?log_error("Parse credentials: ~p failed", [Creds]),
            {error, authentication}
    end.
