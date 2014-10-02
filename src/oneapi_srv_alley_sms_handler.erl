-module(oneapi_srv_alley_sms_handler).

-behaviour(oneapi_srv_gen_sms_handler).

-include("oneapi_srv.hrl").
-include("oneapi_srv_sms_handler_spec.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("alley_services/include/alley_services.hrl").

%% oneapi_srv_gen_sms_handler callbacks
-export([
    init/1,
    handle_send_sms_req/2,
    handle_delivery_status_req/3,
    handle_delivery_notifications_subscribe/2,
    handle_delivery_notifications_unsubscribe/3,
    handle_retrieve_req/2,
    handle_inbound_subscribe/2,
    handle_inbound_unsubscribe/2
]).

-record(state, {
    creds :: term(),
    response :: term()
}).

%% ===================================================================
%% oneapi_srv_gen_sms_handler callbacks
%% ===================================================================

init(Creds = #credentials{}) ->
    CustomerId = Creds#credentials.customer_id,
    UserId = Creds#credentials.user_id,
    Password = Creds#credentials.password,
    {ok, Response = #k1api_auth_response_dto{}} =
        alley_services_auth:authenticate(CustomerId, UserId, oneapi, Password),
    {ok, #state{creds = Creds, response = Response}}.

handle_send_sms_req(OutboundSms = #outbound_sms{}, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    ?log_debug("Got outbound sms request: ~p", [OutboundSms]),

    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId     = Creds#credentials.user_id,

    %% mandatory
    Recipients = oneapi_srv_utils:reformat_addrs(OutboundSms#outbound_sms.address),
    Originator = oneapi_srv_utils:reformat_addr(OutboundSms#outbound_sms.sender_address),
    Message    = OutboundSms#outbound_sms.message,

    %% optional
    Params = outbound_sms_optional_params(OutboundSms),

    SendReq = #send_req{
        action = send_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = oneapi,
        customer = Customer,
        recipients = Recipients,
        originator = Originator,
        text = Message,
        flash = false,
        smpp_params = Params
    },
    {ok, Result} = alley_services_mt:send(SendReq),
    ?log_debug("Got submit result: ~p", [Result]),

    case Result#send_result.result of
        ok ->
            {ok, Result#send_result.req_id};
        _Error ->
            %% TODO handle errors
            {exception, 'svc0004', [<<"address">>]}
    end.

handle_delivery_status_req(SenderAddr, SendSmsRequestID, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Creds#credentials.user_id,
    ?log_debug("Got delivery status request "
        "[customer: ~p, user: ~p, sender_address: ~p, send_sms_req_id: ~p]",
        [CustomerUUID, UserID, SenderAddr, SendSmsRequestID]),
    SenderAddr2 = alley_services_utils:addr_to_dto(SenderAddr),
    {ok, Response} =
        alley_services_api:get_delivery_status(CustomerUUID, UserID, SendSmsRequestID, SenderAddr2),
    Statuses = Response#k1api_sms_delivery_status_response_dto.statuses,
    %% convert [#k1api_sms_status_dto{}] to [{"dest_addr", "status"}]
    DeliveryStatuses = convert_delivery_statuses(Statuses),
    {ok, DeliveryStatuses}.

handle_retrieve_req(Request = #retrieve_sms_req{}, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    #retrieve_sms_req{
        reg_id = RegID,
        batch_size = BatchSize
    } = Request,
    CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Creds#credentials.user_id,
    ?log_debug("Sending retrieve sms request", []),
    DestAddr = RegID,
    DestAddr2 = alley_services_utils:addr_to_dto(DestAddr),
    {ok, Response} =
        alley_services_api:retrieve_sms(CustomerUUID, UserID, DestAddr2, BatchSize),
    ?log_debug("Response: ~p", [Response]),
    #k1api_retrieve_sms_response_dto{
        messages = MessagesDTO,
        total = Total
    } = Response,
    Messages = lists:map(fun(MessageDTO) ->
        #k1api_retrieved_sms_dto{
            datetime = {MegaSecs, Secs, _MicroSecs},
            sender_addr = SenderAddr,
            message_id = MessageID,
            message = MessageText
        } = MessageDTO,
        UnixEpochDateTime = MegaSecs * 1000000 + Secs,
        #inbound_sms{
            date_time = ac_datetime:unixepoch_to_datetime(UnixEpochDateTime),
            message_id = MessageID,
            message = MessageText,
            sender_addr = SenderAddr#addr.addr}
    end, MessagesDTO),
    ?log_debug("Retrieved messages in EOneAPI format: ~p", [Messages]),
    {ok, Messages, Total}.

handle_delivery_notifications_subscribe(Req, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    ?log_debug("Got delivery notifications subscribe request: ~p", [Req]),
    #delivery_receipt_subscribe{
        sender_addr = SenderAddr,
        notify_url = NotifyUrl,
        correlator = Correlator,
        criteria = _Criteria,
        callback_data = CallbackData
    } = Req,
    CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Creds#credentials.user_id,
    ReqID = uuid:unparse(uuid:generate()),
    case oneapi_srv_db:check_correlator(CustomerUUID, UserID, Correlator, ReqID) of
        ok ->
            ?log_debug("Correlator saved", []),
            SourceAddr = #addr{addr = SenderAddr, ton = 1, npi = 1},
            {ok, _Response} = alley_services_api:subscribe_sms_receipts(
                ReqID, CustomerUUID, UserID, NotifyUrl, SourceAddr, CallbackData),
            {ok, ReqID};
        {correlator_exist, OrigReqID} ->
            ?log_debug("Correlator exist: ~p", [OrigReqID]),
            {ok, OrigReqID}
    end.

handle_delivery_notifications_unsubscribe(_SenderAdress, SubscriptionID, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Creds#credentials.user_id,
    RequestID = uuid:unparse(uuid:generate()),
    {ok, _Resp} = alley_services_api:unsubscribe_sms_receipts(
        RequestID, CustomerUUID, UserID, SubscriptionID),
    ?log_debug("Subscription [id: ~p] was successfully removed", [SubscriptionID]),
    {ok, deleted}.

handle_inbound_subscribe(Req, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    ?log_debug("Got inbound subscribe event: ~p", [Req]),
    CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Creds#credentials.user_id,
    #subscribe_inbound{
        dest_addr = DestAddr,
        notify_url = NotifyURL,
        criteria = Criteria,
        callback_data = CallbackData,
        correlator = Correlator
    } = Req,
    ReqID = uuid:unparse(uuid:generate()),
    ?log_debug("Got correlator: ~p", [Correlator]),
    case oneapi_srv_db:check_correlator(CustomerUUID, UserID, Correlator, ReqID) of
        ok ->
            ?log_debug("Correlator saved", []),
            DestAddr2 = #addr{addr = DestAddr, ton = 1, npi = 1},
            {ok, Resp} = alley_services_api:subscribe_incoming_sms(
                ReqID, CustomerUUID, UserID, DestAddr2,
                NotifyURL, Criteria, Correlator, CallbackData),
            SubscriptionID = Resp#k1api_subscribe_incoming_sms_response_dto.subscription_id,
            ?log_debug("Got subscriptionID: ~p", [SubscriptionID]),
            {ok, SubscriptionID};
        {correlator_exist, OrigReqID} ->
            ?log_debug("Correlator exist: ~p", [OrigReqID]),
            {ok, OrigReqID}
    end.

handle_inbound_unsubscribe(SubscribeID, #state{
    creds = Creds,
    response = #k1api_auth_response_dto{result = {customer, Customer}}
}) ->
    ?log_debug("Got inbound unsubscribe event: ~p", [SubscribeID]),
    CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Creds#credentials.user_id,
    RequestID = uuid:unparse(uuid:generate()),
    {ok, _Resp} = alley_services_api:unsubscribe_incoming_sms(
        RequestID, CustomerUUID, UserID, SubscribeID),
    {ok, deleted}.

%% ===================================================================
%% Internal
%% ===================================================================

outbound_sms_optional_params(OutboundSms = #outbound_sms{}) ->
    Fun =
        fun({Name, Index}, Acc) ->
            case element(Index, OutboundSms) of
                undefined ->
                    Acc;
                Value ->
                    [{Name, Value} | Acc]
            end
        end,
    Params = [{<<"oneapi_client_correlator">>, #outbound_sms.client_correlator},
              {<<"oneapi_notify_url">>, #outbound_sms.notify_url},
              {<<"oneapi_callback_data">>, #outbound_sms.callback_data}],
    lists:foldl(Fun, [], Params).

convert_delivery_statuses(#k1api_sms_status_dto{
    address = Addr, status = Status
}) ->
    {Addr#addr.addr, oneapi_srv_utils:translate_status_name(Status)};
convert_delivery_statuses(Statuses) ->
    [convert_delivery_statuses(Status) || Status <- Statuses].
