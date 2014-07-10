-module(eoneapi).

-include("eoneapi.hrl").
-include_lib("alley_common/include/logging.hrl").

-define(VERSION, "2").

%% API Exports
-export([
    start_service/1,
    build_sms_handle_spec/1,
    deliver_sms_status/1,
    deliver_sms/1,
    exception/4,
    code/3
]).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_service([{term(), term()}]) -> ignore.
start_service(EOneAPIProps) ->
    Addr = proplists:get_value(addr, EOneAPIProps, {0,0,0,0}),
    Port = proplists:get_value(port, EOneAPIProps, 8080),
    AcceptorsNum = proplists:get_value(acceptors_num, EOneAPIProps, 1),
    SmsHandlerSpec = build_sms_handle_spec(EOneAPIProps),

    TransportOpts = [{ip, Addr}, {port, Port}],
    Dispatch = cowboy_router:compile([
        {'_', SmsHandlerSpec ++
            [{'_', eoa_error_handler, []}]
        }
    ]),
    ProtocolOpts = [
        {env, [{dispatch, Dispatch}]}
    ],

    {ok, _Pid} = cowboy:start_http(
        my_http_listener, AcceptorsNum, TransportOpts, ProtocolOpts
    ),
    ?log_info("http server is listening to ~p:~p", [Addr, Port]),
    ok.

-spec build_sms_handle_spec([{any(), any()}]) -> any().
build_sms_handle_spec(EOneAPIProps) ->
    case proplists:get_value(sms_handler, EOneAPIProps, undefined) of
        undefined ->
            [];
        SmsHandler -> [
            {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/requests",
                eoa_sms_handler, [SmsHandler]},
            {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/requests/:request_id/deliveryInfos",
                eoa_sms_handler, [SmsHandler]},
            {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/subscriptions",
                eoa_sms_handler, [SmsHandler]},
            {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/subscriptions/:subscription_id",
                eoa_sms_handler, [SmsHandler]},
            {"/" ++ ?VERSION ++ "/smsmessaging/inbound/registrations/:registration_id/messages",
                eoa_sms_handler, [SmsHandler]},
            {"/" ++ ?VERSION ++ "/smsmessaging/inbound/subscriptions",
                eoa_sms_handler, [SmsHandler]},
            {"/" ++ ?VERSION ++ "/smsmessaging/inbound/subscriptions/:subscription_id",
                eoa_sms_handler, [SmsHandler]}
        ]
    end.

-spec deliver_sms_status(delivery_receipt()) -> {ok, term()} | {error, term()}.
deliver_sms_status(#delivery_receipt{    notify_url = NotifyURL,
                                        callback = CallbackData,
                                        dest_addr = RawDestAddr,
                                        status = Status }) ->
    DestAddr = << <<"tel:+">>/binary, RawDestAddr/binary>>,
    ContentType = "application/json",
    StatusBin = Status,
    Body =
    [{<<"deliveryInfoNotification">>, [
        {<<"callbackData">>, CallbackData},
        {<<"deliveryInfo">>, [
            {<<"address">>, DestAddr},
            {<<"deliveryStatus">>, StatusBin}
        ]}
    ]}],
    JsonBody = jsx:encode(Body),
    httpc:request(    post,
                    {binary_to_list(NotifyURL),    [],    ContentType, JsonBody},
                    [{timeout, 5000}],
                    [{body_format, binary}]    ).

-spec deliver_sms(inbound_sms()) -> {ok, term()} | {error, term()}.
deliver_sms(#inbound_sms{
                            notify_url = NotifyURL,
                            date_time = DateTime,
                            dest_addr = DestAddr,
                            message_id = MessId,
                            message = Message,
                            sender_addr = SenderAddr,
                            callback = CallBack        }) ->
    DateTimeBin = ac_datetime:datetime_to_iso8601(DateTime),
    Body =
    [{<<"inboundSMSMessageNotification">>, [
        {<<"callbackData">>, CallBack},
        {<<"inboundSMSMessage">>, [
            {<<"dateTime">>, DateTimeBin},
            {<<"destinationAddress">>, DestAddr},
            {<<"messageId">>, MessId},
            {<<"message">>, Message},
            {<<"senderAddress">>, SenderAddr}
        ]}
    ]}],
    JsonBody = jsx:encode(Body),
    ContentType = "application/json",
    httpc:request(    post,
                    {binary_to_list(NotifyURL), [], ContentType, JsonBody},
                    [{timeout, 5000}],
                    [{body_format, binary}]).

%% ===================================================================
%% HTTP Response Codes
%% ===================================================================

-spec code(integer(), term(), term()) -> {ok, term(), term()}.
code(500, Req, State) ->
    Body = <<"Internal Server Error">>,
    {ok, Req2} = cowboy_req:reply(500, [], Body, Req),
    {ok, Req2, State};

code(401, Req, State) ->
    Headers = [{'Www-Authenticate', <<"Basic">>}],
    Body = <<"Authentication failure, check your authentication details">>,
    {ok, Req2} = cowboy_req:reply(401, Headers, Body, Req),
    {ok, Req2, State};

code(404, Req, State) ->
    Body = <<"Not found: mistake in the host or path of the service URI">>,
    {ok, Req2} = cowboy_req:reply(404, [], Body, Req),
    {ok, Req2, State}.

%% ===================================================================
%% Exceptions
%% ===================================================================

-spec exception(ExceptionTag :: atom(), Req :: term(), State :: term(), Variables :: [term()]) ->
    {ok, Req2 :: term(), State :: term()}.
exception(ExceptionTag, Variables, Req, State) ->
    {ok, Body, Code} =
        oneapi_srv_exception:exception_body_and_code(ExceptionTag, Variables),
    Headers = [{'Content-Type', <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
    {ok, Req2, State}.

%% ===================================================================
%% Internal Functions
%% ===================================================================
