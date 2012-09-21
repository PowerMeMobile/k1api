-module(k1api_sms_handler).

-behaviour(eoa_sms_handler).

-include_lib("eoneapi/include/eoneapi_sms.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include("logging.hrl").

%% API
-export([
	deliver_status/3,
	deliver_sms/3
	]).

%% Eoneapi sms handler callbacks
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
	customer :: term()
	}).

%% API

-spec deliver_status(term(), term(), term()) -> ok.
deliver_status(NotifyURL, NotificationFormat, Req) ->
	eoneapi:deliver_sms_status(NotifyURL, NotificationFormat, Req).

-spec deliver_sms(term(), term(), term()) -> ok.
deliver_sms(NotifyURL, NotificationFormat, Req) ->
	eoneapi:deliver_sms(NotifyURL, NotificationFormat, Req).

%% ===================================================================
%% Eoneapi sms handler callbacks
%% ===================================================================

init(Creds = #credentials{}) ->
	?log_debug("Credentials: ~p", [Creds]),
	case k1api_auth_srv:authenticate(Creds) of
		{ok, Customer = #funnel_auth_response_customer_dto{}} ->
			?log_debug("Customer: ~p", [Customer]),
			{ok, #state{creds = Creds, customer = Customer}};
	   	{error, Error} ->
			?log_error("~p", [Error]),
			{error, Error}
	end.

handle_send_sms_req(OutboundSms = #outbound_sms{},
		#state{customer = Customer, creds = Creds}) ->
	?log_debug("Got outbound sms request:  ~p", [OutboundSms]),
	case k1api_outbound_sms_srv:send(OutboundSms, Customer, Creds) of
		{ok, RequestIDStr} ->
			?log_debug("Message sucessfully sent [id: ~p]", [RequestIDStr]),
			{ok, RequestIDStr};
		{exist, RequestIDStr} ->
			?log_debug("Message already sent [id: ~p]", [RequestIDStr]),
			{ok, RequestIDStr};
		{error, Error} ->
			?log_debug("Send message error: ~p", [Error]),
			{error, Error}
	end.

handle_delivery_status_req(SenderAddress, SendSmsRequestIdStr,
		#state{creds = Creds, customer = Customer}) ->
	SendSmsRequestId = uuid:to_binary(SendSmsRequestIdStr),
	#funnel_auth_response_customer_dto{
		uuid = CustomerUUID
	} = Customer,
	#credentials{user = User} = Creds,
	?log_debug("Got delivery status request "
		"[customer: ~p, user: ~p, sender_address: ~p, send_sms_req_id: ~p]",
		[CustomerUUID, User, SenderAddress, SendSmsRequestId]),
	{ok, Statuses} = k1api_delivery_status_srv:get(CustomerUUID, User, SenderAddress, SendSmsRequestId),

	%% convert [#k1api_sms_status_dto{}] to [{"dest_addr", "status"}]
	DeliveryStatuses = convert_delivery_statuses(Statuses),

	{ok, DeliveryStatuses}.

handle_delivery_notifications_subscribe(Req, _State = #state{}) ->
	?log_debug("Req: ~p", [Req]),
	SubscriptionId = "sub789",
	{ok, SubscriptionId}.

handle_delivery_notifications_unsubscribe(SenderAdress, SubscriptionId, _State = #state{}) ->
	?log_debug("SenderAdress: ~p", [SenderAdress]),
	?log_debug("SubscriptionId: ~p", [SubscriptionId]),
	{ok, deleted}.

handle_retrieve_req(#retrieve_sms_req{
								registration_id = RegId,
								batch_size = BatchSize
								}, _State = #state{}) ->
	?log_debug("RegId: ~p", [RegId]),
	?log_debug("BatchSize: ~p", [BatchSize]),
	Pending = 40,
	IncomingSmsList = [#inbound_sms{
						date_time = {{2012, 01, 11}, {12, 13, 00}},
						message_id = "msg1",
						message = "first test message",
						sender_address = "375298765425"},
						#inbound_sms{
						date_time = {{2012, 01, 11}, {13, 15, 00}},
						message_id = "msg2",
						message = "the second test message",
						sender_address = "375298789463"}
						],
	{ok, IncomingSmsList, Pending}.

handle_inbound_subscribe(Req, #state{creds = Creds, customer = Customer}) ->
	?log_debug("Inbound subscribe event.", []),
	#funnel_auth_response_customer_dto{
		uuid = _CustomerID
		} = Customer,
	#credentials{user = _UserID} = Creds,
	#subscribe_inbound{
		%% destination_address = DestAddr,
		%% notify_url = NotifyURL,
		%% criteria = Criteria, % opt
		%% callback_data = CallbackData, % opt
		%% client_correlator = ClientCorrelator % opt
		} = Req,
	{ok, _IncomingQ} = application:get_env(k1api, incoming_queue),
	SubscriptionId = uuid:to_string(uuid:newid()),
	%% SubscribeEvent = #subscribeevent{
	%% 	subscribe_id = SubscriptionId,
	%% 	queue_name = IncomingQ,
	%% 	customer_id = CustomerID,
	%% 	user_id = UserID,
	%% 	type = incomingSMSReceiver,
	%% 	destination_addr = DestAddr,
    %%     notify_url = NotifyURL,
    %%     criteria = Criteria,
    %%     notification_format = undefined,
    %%     client_correlator = ClientCorrelator,
    %%     callback_data = CallbackData
	%% },
	%% SubscribeEventBin = oa_pb:encode_subscribeevent(SubscribeEvent),
	%% k1api_subscription_srv:subscribe(SubscribeEventBin),
	?log_debug("SubscriptionId: ~p", [SubscriptionId]),
	SubscriptionId = "SubscriptionId",
	{ok, SubscriptionId}.

handle_inbound_unsubscribe(_SubscriptionId, _State = #state{}) ->
	{ok, deleted}.

%% ===================================================================
%% Internal
%% ===================================================================

convert_delivery_statuses(Status = #k1api_sms_status_dto{}) ->
	#k1api_sms_status_dto{
		address = AddrDTO,
		status = StatusNameDTO
	} = Status,
	{binary_to_list(AddrDTO#addr_dto.addr), translate_status_name(StatusNameDTO)};
convert_delivery_statuses(Statuses) ->
	[convert_delivery_statuses(Status) || Status <- Statuses].

translate_status_name(submitted) ->
	"MessageWaiting";
translate_status_name(success_waiting_delivery) ->
	"MessageWaiting";
translate_status_name(success_no_delivery) ->
	"DeliveryImpossible";
translate_status_name(failure) ->
	"DeliveryUncertain";
translate_status_name(enroute) ->
	"Enroute";
translate_status_name(delivered) ->
	"DeliveredToTerminal";
translate_status_name(expired) ->
	"DeliveryImpossible";
translate_status_name(deleted) ->
	"Deleted";
translate_status_name(undeliverable) ->
	"DeliveryImpossible";
translate_status_name(accepted) ->
	"DeliveredToNetwork";
translate_status_name(unknown) ->
	"DeliveryUncertain";
translate_status_name(rejected) ->
	"Rejected";
translate_status_name(unrecognized) ->
	"Unrecognized";
translate_status_name(Any) ->
	erlang:error({badarg, Any}).
