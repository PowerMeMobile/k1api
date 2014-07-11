-module(oneapi_srv_protocol).

-include("application.hrl").
-include("oneapi_srv.hrl").
-include_lib("alley_common/include/logging.hrl").

-define(VERSION, "2").

%% API
-export([
    init/0,
    build_sms_handle_spec/1,
    deliver_sms_status/1,
    deliver_sms/1,
    exception/3,
    exception/4,
    code/3
]).

%% ===================================================================
%% API
%% ===================================================================

-spec init() -> ok.
init() ->
    {ok, Addr} = application:get_env(?APP, http_addr),
    {ok, Port} = application:get_env(?APP, http_port),
    {ok, AcceptorsNum} = application:get_env(?APP, http_acceptors_num),

    SmsHandlerSpec = build_sms_handle_spec(oneapi_srv_alley_sms_handler),

    TransOpts = [{ip, Addr}, {port, Port}],
    Dispatch = cowboy_router:compile([
        {'_', SmsHandlerSpec ++
            [{'_', oneapi_srv_error_handler, []}]
        }
    ]),
    ProtoOpts = [
        {env, [{dispatch, Dispatch}]}
    ],

    {ok, _Pid} = cowboy:start_http(
        my_http_listener, AcceptorsNum, TransOpts, ProtoOpts
    ),
    ?log_info("http server is listening to ~p:~p", [Addr, Port]),
    ok.

-spec build_sms_handle_spec(atom()) -> [term()].
build_sms_handle_spec(SmsHandler) -> [
    {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/requests",
        oneapi_srv_gen_sms_handler, [SmsHandler]},
    {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/requests/:request_id/deliveryInfos",
        oneapi_srv_gen_sms_handler, [SmsHandler]},
    {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/subscriptions",
        oneapi_srv_gen_sms_handler, [SmsHandler]},
    {"/" ++ ?VERSION ++ "/smsmessaging/outbound/:sender_addr/subscriptions/:subscription_id",
        oneapi_srv_gen_sms_handler, [SmsHandler]},
    {"/" ++ ?VERSION ++ "/smsmessaging/inbound/registrations/:registration_id/messages",
        oneapi_srv_gen_sms_handler, [SmsHandler]},
    {"/" ++ ?VERSION ++ "/smsmessaging/inbound/subscriptions",
        oneapi_srv_gen_sms_handler, [SmsHandler]},
    {"/" ++ ?VERSION ++ "/smsmessaging/inbound/subscriptions/:subscription_id",
        oneapi_srv_gen_sms_handler, [SmsHandler]}
].

-spec deliver_sms_status(delivery_receipt()) -> {ok, term()} | {error, term()}.
deliver_sms_status(#delivery_receipt{
    notify_url = NotifyURL,
    callback_data = CallbackData,
    dest_addr = RawDestAddr,
    status = Status
}) ->
    DestAddr = << <<"tel:+">>/binary, RawDestAddr/binary>>,
    ContentType = "application/json",
    StatusBin = Status,
    Body = [
        {<<"deliveryInfoNotification">>, [
            {<<"callbackData">>, CallbackData},
            {<<"deliveryInfo">>, [
                {<<"address">>, DestAddr},
                {<<"deliveryStatus">>, StatusBin}
            ]}
        ]}
    ],
    JsonBody = jsx:encode(Body),
    httpc:request(post,
        {binary_to_list(NotifyURL), [], ContentType, JsonBody},
        [{timeout, 5000}], [{body_format, binary}]).

-spec deliver_sms(inbound_sms()) -> {ok, term()} | {error, term()}.
deliver_sms(#inbound_sms{
    notify_url = NotifyURL,
    date_time = DateTime,
    dest_addr = DestAddr,
    message_id = MessageId,
    message = Message,
    sender_addr = SenderAddr,
    callback_data = CallBackData
}) ->
    DateTimeBin = ac_datetime:datetime_to_iso8601(DateTime),
    Body = [
        {<<"inboundSMSMessageNotification">>, [
            {<<"callbackData">>, CallBackData},
            {<<"inboundSMSMessage">>, [
                {<<"dateTime">>, DateTimeBin},
                {<<"destinationAddress">>, DestAddr},
                {<<"messageId">>, MessageId},
                {<<"message">>, Message},
                {<<"senderAddress">>, SenderAddr}
            ]}
        ]}
    ],
    JsonBody = jsx:encode(Body),
    ContentType = "application/json",
    httpc:request(post,
        {binary_to_list(NotifyURL), [], ContentType, JsonBody},
        [{timeout, 5000}], [{body_format, binary}]).

%% ===================================================================
%% HTTP Response Codes
%% ===================================================================

-spec code(integer(), term(), term()) -> {ok, term(), term()}.
code(500, Req, State) ->
    Body = <<"Internal Server Error">>,
    {ok, Req2} = cowboy_req:reply(500, [], Body, Req),
    {ok, Req2, State};

code(401, Req, State) ->
    Headers = [{<<"www-authenticate">>, <<"Basic">>}],
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

-spec exception(ExceptionTag::atom(), Req::term(), State::term()) ->
    {ok, Req2::term(), State::term()}.
exception(ExceptionTag, Req, State) ->
    exception(ExceptionTag, [], Req, State).

-spec exception(ExceptionTag::atom(), ExceptionVars::[term()], Req::term(), State::term()) ->
    {ok, Req2::term(), State::term()}.
exception(ExceptionTag, ExceptionVars, Req, State) ->
    {ok, Body, Code} =
        oneapi_srv_exception:exception_body_and_code(ExceptionTag, ExceptionVars),
    Headers = [{<<"content-type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
    {ok, Req2, State}.
