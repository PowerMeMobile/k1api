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

-record(state, {
    req         :: term(),
    mod         :: atom(),
    mstate      :: term(),
    creds       :: credentials(),
    sender_addr :: binary(),
    thendo      :: fun(),
    thendo_args :: term()
}).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================

-callback init(credentials()) ->
    {ok, state()} |
    {error, denied}.

-callback handle_send_outbound(outbound_sms(), state()) ->
    {ok, request_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_query_delivery_status(sender_address(), request_id(), state()) ->
    {ok, sms_delivery_statuses()}  |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_subscribe_delivery_notifications(subscribe_delivery_notifications(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_unsubscribe_delivery_notifications(sender_address(), subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_retrieve_inbound(retrieve_sms_req(), state()) ->
    {ok, [inbound_sms()], pending_sms()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_subscribe_inbound_notifications(subscribe_inbound(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_unsubscribe_inbound_notifications(subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

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
        {error, unauthorized} ->
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
    AfterInit = fun(Args, St) -> process_subscribe_delivery_notifications(Args, St) end,
    Args = [],
    do_init(State#state{
        req = Req2,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"DELETE">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _RawSenderAddr, <<"subscriptions">>, _SubId],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    {SubId, Req3} = cowboy_req:binding(subscription_id, Req2),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_unsubscribe_delivery_notifications(Args, St) end,
    Args = SubId,
    do_init(State#state{
        req = Req3,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"GET">>,
    [_Ver, <<"smsmessaging">>, <<"inbound">>, <<"registrations">>, _RegId, <<"messages">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RegId, Req2} = cowboy_req:binding(registration_id, Req),
    AfterInit = fun(Args, St) -> process_retrieve_outbound(Args,St) end,
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
    AfterInit = fun(Args, St) -> process_subscribe_inbound_notifications(Args, St) end,
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
    AfterInit = fun(Args, St) -> process_unsubscribe_inbound_notifications(Args, St) end,
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
        {error, denied} ->
            oneapi_srv_protocol:code(401, Req, State)
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
            ContentType = <<"application/json">>,
            Location = build_resource_url(Req2, ReqId),
            Body = [
                {<<"resourceReference">>, [
                    {<<"resourceURL">>, Location}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [{<<"content-type">>, ContentType}, {<<"location">>, Location}],
            {ok, Req3} = cowboy_req:reply(201, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req2, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req2, State)
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
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Subscribe to delivery notifications
%% ===================================================================

process_subscribe_delivery_notifications(_, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState,
    sender_addr = SenderAddr
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    Request = #subscribe_delivery_notifications{
        %% mandatory
        notify_url = gv(QsVals, <<"notifyURL">>),
        %% optional
        client_correlator = gv(QsVals, <<"clientCorrelator">>),
        criteria          = gv(QsVals, <<"criteria">>),
        callback_data     = gv(QsVals, <<"callbackData">>),
        %% other
        sender_addr   = SenderAddr
    },
    case Mod:handle_subscribe_delivery_notifications(Request, MState) of
        {ok, SubscribeId} ->
            CallBackData = gv(QsVals, <<"callbackData">>),
            NotifyURL = gv(QsVals, <<"notifyURL">>),
            Location = build_resource_url(Req, SubscribeId),
            ContentType = <<"application/json">>,
            Criteria = gv(QsVals, <<"criteria">>),
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
            Headers = [{<<"content-type">>, ContentType}, {<<"location">>, Location}],
            {ok, Req3} = cowboy_req:reply(201, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req2, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req2, State)
    end.

%% ===================================================================
%% Unsubscribe delivery notifications
%% ===================================================================

process_unsubscribe_delivery_notifications(SubscribeId, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState,
    sender_addr = Addr
}) ->
    case Mod:handle_unsubscribe_delivery_notifications(Addr, SubscribeId, MState) of
        {ok, deleted} ->
            {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Retrieve outbound
%% ===================================================================

process_retrieve_outbound(RegId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    RetrieveSmsReq = #retrieve_sms_req{
        reg_id = RegId,
        batch_size = giv(QsVals, <<"maxBatchSize">>)
    },
    case Mod:handle_retrieve_req(RetrieveSmsReq, MState) of
        {ok, ListOfInboundSms, PendingSms} ->
            Messages =
                lists:map(fun(#inbound_sms{
                                date_time = DateTime,
                                message_id = MessIdBin,
                                message = MessageTextBin,
                                sender_addr = SenderAddrBin})->
                    DateTimeBin = ac_datetime:datetime_to_iso8601(DateTime),
                    LocationUrl = build_resource_url(Req, MessIdBin),
                    [{<<"dateTime">>, DateTimeBin},
                    {<<"destinationAddress">>, RegId},
                    {<<"messageId">>, MessIdBin},
                    {<<"message">>, MessageTextBin},
                    {<<"resourceURL">>, LocationUrl},
                    {<<"senderAddress">>, SenderAddrBin}]
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
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req2, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req2, State)
    end.

%% ===================================================================
%% Subscribe inbound notifications
%% ===================================================================

process_subscribe_inbound_notifications( _, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState
}) ->
    {QsVals, Req2} = get_qs_vals(Req),
    SubscribeInbound = #subscribe_inbound{
        dest_addr     = convert_addr(gv(QsVals, <<"destinationAddress">>)),
        notify_url    = gv(QsVals, <<"notifyURL">>),
        criteria      = gv(QsVals, <<"criteria">>),
        callback_data = gv(QsVals, <<"callbackData">>),
        correlator    = gv(QsVals, <<"clientCorrelator">>)
    },
    case Mod:handle_subscribe_inbound_notifications(SubscribeInbound, MState) of
        {ok, SubId} ->
            Location = build_resource_url(Req, SubId),
            ContentType = <<"application/json">>,
            Body = [
                {<<"resourceReference">>, [
                    {<<"resourceURL">>, Location}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [{<<"location">>, Location}, {<<"content-type">>, ContentType}],
            {ok, Req3} = cowboy_req:reply(201, Headers, JsonBody, Req2),
            {ok, Req3, State#state{req = Req3}};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Unsubscribe inbound notifications
%% ===================================================================

process_unsubscribe_inbound_notifications(SubId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req
}) ->
    case Mod:handle_unsubscribe_inbound_notifications(SubId, MState) of
        {ok, deleted} ->
            {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
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
    {ok, Body, Req2} = cowboy_req:body(800000, Req),
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
            binary_to_integer(Value)
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
    {error, unauthorized};
parse_credential_header(Header, Delimiter) ->
    List = binary:split(Header, [<<"Basic">>, <<" ">>], [global]),
    [Base64] = [Item || Item <- List, Item =/= <<>>],
    Creds = base64:decode(Base64),
    [CustomerId, UserId, Password] =
        binary:split(Creds, [<<":">>] ++ Delimiter, [global]),
    {ok, {CustomerId, UserId, Password}}.
