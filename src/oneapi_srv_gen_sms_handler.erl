-module(oneapi_srv_gen_sms_handler).

-behaviour(cowboy_http_handler).

%% cowboy_http_handler callbacks
-export([
    init/3,
    handle/2,
    terminate/3
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

-callback handle_send_sms_req(outbound_sms(), state()) ->
    {ok, request_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_delivery_status_req(sender_address(), request_id(), state()) ->
    {ok, sms_delivery_statuses()}  |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_delivery_notifications_subscribe(delivery_receipt_subscribe(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_delivery_notifications_unsubscribe(sender_address(), subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_retrieve_req(retrieve_sms_req(), state()) ->
    {ok, [inbound_sms()], pending_sms()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_inbound_subscribe(subscribe_inbound(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-callback handle_inbound_unsubscribe(subscription_id(), state()) ->
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
        {ok, {CustId, UserId, Pass}} ->
            Creds = #credentials{customer_id = CustId, user_id = UserId, password = Pass},
            [<<>> | Segments] = binary:split(Path, <<"/">>, [global]),
            handle_req(Method, Segments, State#state{creds = Creds});
        {error, unauthorized} ->
            oneapi_srv_protocol:code(401, Req, [])
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Parsing http requests
%% ===================================================================

handle_req(<<"POST">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _RawSenderAddr, <<"requests">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_outbound_sms_req(Args, St) end,
    Args = [],
    do_init(State#state{
        req = Req2,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"GET">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _ServerAddr, <<"requests">>, _ReqId, <<"deliveryInfos">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    {ReqId, Req3} = cowboy_req:binding(request_id, Req2),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_delivery_status_req(Args, St) end,
    Args = ReqId,
    do_init(State#state{
        req = Req3,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"POST">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _SenderAddr, <<"subscriptions">>],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_sms_delivery_report_subscribe_req(Args, St) end,
    Args = [],
    do_init(State#state{
        req = Req2,
        sender_addr = SenderAddr,
        thendo = AfterInit,
        thendo_args = Args,
        creds = Creds
    });

handle_req(<<"DELETE">>,
    [_Ver, <<"smsmessaging">>, <<"outbound">>, _SenderAddr, <<"subscriptions">>, _SubId],
    State = #state{req = Req, creds = Creds}
) ->
    {RawSenderAddr, Req2} = cowboy_req:binding(sender_addr, Req),
    {SubId, Req3} = cowboy_req:binding(subscription_id, Req2),
    SenderAddr = convert_addr(RawSenderAddr),
    AfterInit = fun(Args, St) -> process_sms_delivery_report_unsubscribe_req(Args, St) end,
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
    AfterInit = fun(Args, St) -> process_retrieve_sms_req(Args,St) end,
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
    AfterInit = fun(Args, St) -> process_sms_delivery_subscribe_req(Args, St) end,
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
    AfterInit = fun(Args, St) -> process_sms_delivery_unsubscribe_req(Args, St) end,
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
    InitResult = Mod:init(Creds),
    case InitResult of
        {ok, MState} ->
            Fun(Args, State#state{mstate = MState});
        {error, denied} ->
            oneapi_srv_protocol:code(401, Req, State)
    end.

%% ===================================================================
%% Outbound sms request
%% ===================================================================

process_outbound_sms_req(_, State = #state{
    mstate = MState,
    mod = Mod,
    req = Req,
    sender_addr = _Addr
}) ->
    {ok, ReqPropList} = get_prop_list(Req),
    SendSmsReq = #outbound_sms{
        dest_addr     = gmv(ReqPropList, <<"address">>),
        sender_addr   = gv(ReqPropList, <<"senderAddress">>),
        message       = gv(ReqPropList, <<"message">>),
        sender_name   = gv(ReqPropList, <<"senderName">>),
        notify_url    = gv(ReqPropList, <<"notifyURL">>),
        correlator    = gv(ReqPropList, <<"clientCorrelator">>),
        callback_data = gv(ReqPropList, <<"callbackData">>)
    },
    case Mod:handle_send_sms_req(SendSmsReq, MState) of
        {ok, ReqId} ->
            ContentType = <<"application/json">>,
            Location = build_resource_url(Req, ReqId),
            Body = [
                {<<"resourceReference">>, [
                    {<<"resourceURL">>, Location}
                ]}
            ],
            JsonBody = jsx:encode(Body),
            Headers = [{<<"content-type">>, ContentType}, {<<"location">>, Location}],
            {ok, Req2} = cowboy_req:reply(201, Headers, JsonBody, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Delivery Status Request
%% ===================================================================

process_delivery_status_req(ReqId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req,
    sender_addr = SenderAddr
}) ->
    Response = Mod:handle_delivery_status_req(SenderAddr, ReqId, MState),
    case Response of
        {ok, ResponseList} ->
            Reports =
                lists:map(fun({Address, DeliveryStatus})->
                [{<<"address">>, Address}, {<<"deliveryStatus">>, DeliveryStatus}]
                end, ResponseList),
            Resource = build_resource_url(Req),
            Body =
            [{<<"deliveryInfoList">>, [
                {<<"deliveryInfo">>, Reports},
                {<<"resourceURL">>, Resource}
            ]}],
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
%% Start subscribe to SMS delivery notifications
%% ===================================================================

process_sms_delivery_report_subscribe_req(_, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState,
    sender_addr = Addr
}) ->
    {ok, ReqPropList} = get_prop_list(Req),
    Request = #delivery_receipt_subscribe{
        sender_addr   = Addr,
        notify_url    = gv(ReqPropList, <<"notifyURL">>),
        correlator    = gv(ReqPropList, <<"clientCorrelator">>),
        criteria      = gv(ReqPropList, <<"criteria">>),
        callback_data = gv(ReqPropList, <<"callbackData">>)
    },
    case Mod:handle_delivery_notifications_subscribe(Request, MState) of
        {ok, SubscribeId} ->
            CallBackData = gv(ReqPropList, <<"callbackData">>),
            NotifyURL = gv(ReqPropList, <<"notifyURL">>),
            Location = build_resource_url(Req, SubscribeId),
            ContentType = <<"application/json">>,
            Criteria = gv(ReqPropList, <<"criteria">>),
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
            {ok, Req2} = cowboy_req:reply(201, Headers, JsonBody, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Stop the subscription to delivery notifications
%% ===================================================================

process_sms_delivery_report_unsubscribe_req(SubscribeId, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState,
    sender_addr = Addr
}) ->
    Result = Mod:handle_delivery_notifications_unsubscribe(Addr, SubscribeId, MState),
    case Result of
        {ok, deleted} ->
            {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Retrieve messages sent to your Web application
%% ===================================================================

process_retrieve_sms_req(RegId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req
}) ->
    {ok, ReqPropList} = get_prop_list(Req),
    RetrieveSmsReq = #retrieve_sms_req{
        reg_id = RegId,
        batch_size = giv(ReqPropList, <<"maxBatchSize">>)
    },
    Result = Mod:handle_retrieve_req(RetrieveSmsReq, MState),
    case Result of
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
            {ok, Req2} = cowboy_req:reply(200, Headers, JsonBody, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Subscribe to notifications of messages sent to your application
%% ===================================================================

process_sms_delivery_subscribe_req( _, State = #state{
    req = Req,
    mod = Mod,
    mstate = MState
}) ->
    {ok, ReqPropList} = get_prop_list(Req),
    SubscribeInbound = #subscribe_inbound{
        dest_addr     = convert_addr(gv(ReqPropList, <<"destinationAddress">>)),
        notify_url    = gv(ReqPropList, <<"notifyURL">>),
        criteria      = gv(ReqPropList, <<"criteria">>),
        callback_data = gv(ReqPropList, <<"callbackData">>),
        correlator    = gv(ReqPropList, <<"clientCorrelator">>)
    },
    case Mod:handle_inbound_subscribe(SubscribeInbound, MState) of
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
            {ok, Req2} = cowboy_req:reply(201, Headers, JsonBody, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Stop the subscription to message notifications
%% ===================================================================

process_sms_delivery_unsubscribe_req(SubId, State = #state{
    mod = Mod,
    mstate = MState,
    req = Req
}) ->
    case Mod:handle_inbound_unsubscribe(SubId, MState) of
        {ok, deleted} ->
            {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
            {ok, Req2, State};
        {exception, Exception} ->
            oneapi_srv_protocol:exception(Exception, Req, State);
        {exception, Exception, Vars} ->
            oneapi_srv_protocol:exception(Exception, Vars, Req, State)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

get_prop_list(Req) ->
    {Method, Req2} = cowboy_req:method(Req),
    ReqPropList =
        case Method of
            <<"POST">> ->
                {ok, BodyQs, Req3} = cowboy_req:body_qs(Req2),
                {QsVals, _Req4} = cowboy_req:qs_vals(Req3),
                BodyQs ++ QsVals;
            _Any ->
                {QsVals, _Req3} = cowboy_req:qs_vals(Req2),
                QsVals
        end,
    {ok, ReqPropList}.

convert_addr(<<"tel:+", Bin/binary>>) ->
    Bin;
convert_addr(Bin) when is_binary(Bin) ->
    Bin.

%% Return integer value from proplist of request
giv(ReqPropList, Key) ->
    case gv(ReqPropList, Key) of
        undefined -> undefined;
        Value -> list_to_integer(binary_to_list(Value))
    end.

gv(ReqPropList, Key) ->
    case lists:keytake(Key, 1, ReqPropList) of
        {value, {_, Value}, _TupleList2} -> Value;
        _ -> undefined
    end.

gmv(ReqPropList, Key) ->
    lists:flatten(
        lists:map(fun({K, V})->
            case K of
                Key -> V;
                _ -> []
            end
        end, ReqPropList)).

build_resource_url(Req) ->
    build_resource_url(Req, <<>>).
build_resource_url(Req, ItemId) when is_binary(ItemId) ->
    {RawHost, _} = cowboy_req:host(Req),
    {RawPath, _} = cowboy_req:path(Req),
    {Port, _} = cowboy_req:port(Req),
    BitstringPort = list_to_binary(integer_to_list(Port)),
    Protocol = <<"http://">>,
    ReqIdBin = case ItemId of <<>> -> <<>>; Any -> << <<"/">>/binary, Any/binary>> end,
    <<Protocol/binary, RawHost/binary, <<":">>/binary, BitstringPort/binary, RawPath/binary, ReqIdBin/binary>>.

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
    RawList = binary:split(Header, [<<"Basic">>, <<" ">>],[global]),
    [Base64Bin] = lists:filter(fun(Elem) -> Elem =/= <<>> end, RawList),
    CredsBin = base64:decode(Base64Bin),
    [CustIdBin, UserBin, PassBin] = binary:split(CredsBin, [<<":">>] ++ Delimiter, [global]),
    {ok, {CustIdBin, UserBin, PassBin}}.