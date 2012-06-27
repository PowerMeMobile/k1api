-module(k1api_sms_handler).

-behaviour(eoa_sms_handler).

-include_lib("eoneapi/include/eoneapi_sms.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("k1api_proto/include/oa_pb.hrl").
-include_lib("k1api_proto/include/FunnelAsn.hrl").
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

%% Eoneapi sms handler callbacks

init(Creds = #credentials{}) ->
	?log_debug("Credentials: ~p", [Creds]),
	case k1api_auth_srv:authenticate(Creds) of
		{ok, Customer = #'Customer'{}} ->
			?log_debug("Customer: ~p", [Customer]),
			{ok, #state{creds = Creds, customer = Customer}};
	   	{error, Error} ->
			?log_error("~p", [Error]),
			{error, Error}
	end.

handle_send_sms_req(OutboundSms = #outbound_sms{
										% address = Address,
										% sender_address = SenderAddr,
										% message = Message,
										% sender_name = SenderName, % opt
										% notify_url = NotifyURL, % opt
										% client_correlator = Correlator, %opt
										% callback_data = Callback % opt
										}, #state{}) ->
	?log_debug("OutboundSms: ~p", [OutboundSms]),
	{ok, RequestId} = k1api_batch_srv:send_sms(OutboundSms),
%	RequestId = "mes123",
	{ok, RequestId}.

handle_delivery_status_req(SenderAdress, RequestId, _State = #state{}) ->
	?log_debug(": ~p", [SenderAdress]),
	?log_debug(": ~p", [RequestId]),
	DeliveryStatuses = [{"1350000001", "MessageWaiting"}, {"1350000999", "MessageWaiting"}],
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
	#'Customer'{
		uuid = CustomerID
		} = Customer,
	#credentials{user = UserID} = Creds,
	#subscribe_inbound{
		destination_address = DestAddr,
		notify_url = NotifyURL,
		criteria = Criteria, % opt
		callback_data = CallbackData, % opt
		client_correlator = ClientCorrelator % opt
		} = Req,
	{ok, IncomingQ} = application:get_env(k1api, incoming_queue),
	SubscriptionId = k1api_uuid:string_id(),
	SubscribeEvent = #subscribeevent{
		subscribe_id = SubscriptionId,
		queue_name = IncomingQ,
		customer_id = CustomerID,
		user_id = UserID,
		type = incomingSMSReceiver,
		destination_addr = DestAddr,
        notify_url = NotifyURL,
        criteria = Criteria,
        notification_format = undefined,
        client_correlator = ClientCorrelator,
        callback_data = CallbackData
	},
	SubscribeEventBin = oa_pb:encode_subscribeevent(SubscribeEvent),
	k1api_subscription_srv:subscribe(SubscribeEventBin),
	?log_debug("SubscriptionId: ~p", [SubscriptionId]),
	{ok, SubscriptionId}.

handle_inbound_unsubscribe(_SubscriptionId, _State = #state{}) ->
	{ok, deleted}.

%% Local Functions Definitions

