-module(k1api_sms_handler).
-behaviour(eoa_sms_handler).
-compile([{parse_transform, lager_transform}]).
-include("logging.hrl").
-include_lib("eoneapi/include/eoneapi_sms.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").

%% API
-export([
	deliver_status/3,
	deliver_sms/3
	]).

%% Eoneapi sms handler callbacks
-export([
	init/1,
	handle_send_sms_req/3,
	handle_delivery_status_req/4,
	handle_delivery_notifications_subscribe/3,
	handle_delivery_notifications_unsubscribe/4,
	handle_retrieve_req/3,
	handle_inbound_subscribe/3,
	handle_inbound_unsubscribe/3
	]).

-record(state, {
	}).

%% API

-spec deliver_status(term(), term(), term()) -> ok. 
deliver_status(NotifyURL, NotificationFormat, Req) ->
	eoneapi:deliver_sms_status(NotifyURL, NotificationFormat, Req).
	
-spec deliver_sms(term(), term(), term()) -> ok. 
deliver_sms(NotifyURL, NotificationFormat, Req) ->
	eoneapi:deliver_sms(NotifyURL, NotificationFormat, Req).

%% Eoneapi sms handler callbacks

init(Credentials = #credentials{
			system_id = SystemId,
			user = User,
			password = Password,
			type = Type
			}) ->
	?log_debug("Credentials: ~p", [Credentials]),
	{ok, #state{}}.

handle_send_sms_req(#credentials{}, OutboundSms = #outbound_sms{
										address = Address,
										sender_address = SenderAddr,
										message = Message,
										sender_name = SenderName, %opt
										notify_url = NotifyURL, %% opt
										client_correlator = Correlator, %opt
										callback_data = Callback % opt
										}, State = #state{}) ->
	?log_debug("OutboundSms: ~p", [OutboundSms]),
	RequestId = "mes123",
	{ok, RequestId}.

handle_delivery_status_req(Credentials, SenderAdress, RequestId, _State = #state{}) ->
	?log_debug(": ~p", [SenderAdress]),
	?log_debug(": ~p", [RequestId]),
	DeliveryStatuses = [{"1350000001", "MessageWaiting"}, {"1350000999", "MessageWaiting"}],
	{ok, DeliveryStatuses}.

handle_delivery_notifications_subscribe(Credentials, Req, _State = #state{}) ->
	?log_debug("Req: ~p", [Req]),
	SubscriptionId = "sub789",
	{ok, SubscriptionId}.

handle_delivery_notifications_unsubscribe(_Credentials, SenderAdress, SubscriptionId, _State = #state{}) ->
	?log_debug("SenderAdress: ~p", [SenderAdress]),
	?log_debug("SubscriptionId: ~p", [SubscriptionId]),
	{ok, deleted}.

handle_retrieve_req(_Creds, #retrieve_sms_req{
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

handle_inbound_subscribe(_Credentials, _Req, _State = #state{}) ->
	SubscriptionId = "sub678",
	{ok, SubscriptionId}.

handle_inbound_unsubscribe(_Credentials, _SubscriptionId, _State = #state{}) ->
	{ok, deleted}.