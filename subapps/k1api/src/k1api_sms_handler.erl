-module(k1api_sms_handler).

-behaviour(eoa_sms_handler).

-include_lib("eoneapi/include/eoneapi_sms.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include("logging.hrl").

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

%% ===================================================================
%% eoneapi sms handler callbacks
%% ===================================================================

init(Creds = #credentials{}) ->
	?log_debug("Credentials: ~p", [Creds]),
	case k1api_auth_srv:authenticate(Creds) of
		{ok, Customer = #k1api_auth_response_dto{}} ->
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

handle_delivery_status_req(SenderAddress, SendSmsRequestIDStr,
		#state{creds = Creds, customer = Customer}) ->
	SendSmsRequestID = uuid:to_binary(SendSmsRequestIDStr),
	#k1api_auth_response_dto{
		uuid = CustomerUUID
	} = Customer,
	#credentials{user = User} = Creds,
	?log_debug("Got delivery status request "
		"[customer: ~p, user: ~p, sender_address: ~p, send_sms_req_id: ~p]",
		[CustomerUUID, User, SenderAddress, SendSmsRequestID]),
	{ok, Statuses} = k1api_delivery_status_srv:get(CustomerUUID, User, SenderAddress, SendSmsRequestID),

	%% convert [#k1api_sms_status_dto{}] to [{"dest_addr", "status"}]
	DeliveryStatuses = convert_delivery_statuses(Statuses),

	{ok, DeliveryStatuses}.

handle_delivery_notifications_subscribe(Req, State = #state{}) ->
	#state{creds = Creds, customer = Customer} = State,
	#del_rec_subscribe{
		sender_address = {_Protocol, Sender},
		notify_url = Url,
		client_correlator = _Correlator,
		criteria = _Criteria,
		callback_data = Callback
	} = Req,
	#k1api_auth_response_dto{
		uuid = CustomerUUID
		} = Customer,
	ReqID = uuid:newid(),
	#credentials{user = UserID} = Creds,
	ReqDTO = #k1api_subscribe_sms_receipts_request_dto{
		id = ReqID,
		customer_id = CustomerUUID,
		user_id = UserID,
		url = Url,
		dest_addr = #addr_dto{addr = list_to_binary(Sender), ton = 1, npi = 1},
		callback_data = Callback
	},
	?log_debug("ReqDTO: ~p", [ReqDTO]),
	{ok, Bin} = adto:encode(ReqDTO),
	{ok, _RespBin} = k1api_subscription_srv:subscribe_receipts(ReqID, Bin),
	{ok, uuid:to_string(ReqID)}.


handle_delivery_notifications_unsubscribe(_SenderAdress, SubscriptionID, State = #state{}) ->
	SubIDBin = uuid:to_binary(SubscriptionID),
	#state{creds = Creds, customer = Customer} = State,
	#credentials{user = UserID} = Creds,
	#k1api_auth_response_dto{
		uuid = CustomerID
		} = Customer,
	RequestID = uuid:newid(),
	DTO = #k1api_unsubscribe_sms_receipts_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = UserID,
		subscription_id = SubIDBin
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, _RespBin} = k1api_subscription_srv:unsubscribe_receipts(RequestID, Bin),
	?log_debug("Subscription [id: ~p] was successfully removed", [SubscriptionID]),
	{ok, deleted}.

handle_retrieve_req(Request = #retrieve_sms_req{}, State = #state{}) ->
	#retrieve_sms_req{
		registration_id = RegID,
		batch_size = BatchSizeStr
	} = Request,
	#state{creds = Creds, customer = Customer} = State,
	#k1api_auth_response_dto{
		uuid = CustomerUUID
	} = Customer,
	#credentials{user = UserID} = Creds,
	BatchSize = list_to_integer(binary_to_list(BatchSizeStr)),
	?log_debug("Sending retrieve sms request", []),
	DestinationAddress = RegID,
	{ok, Response} =
		k1api_retrieve_sms_srv:get(CustomerUUID, UserID, convert_addr(DestinationAddress), BatchSize),
	?log_debug("Response: ~p", [Response]),
	#k1api_retrieve_sms_response_dto{
		messages = MessagesDTO,
		total = Total
	} = Response,
	Messages = lists:map(fun(MessageDTO) ->
		#k1api_retrieved_sms_dto{
			datetime = DateTime,
			sender_addr = SenderAddr,
			message_id = MessageID,
			message = MessageText
		} = MessageDTO,
		#inbound_sms{
			date_time = k_datetime:unix_epoch_to_datetime(DateTime),
			message_id = uuid:to_string(MessageID),
			message = binary_to_list(MessageText),
			sender_address = binary_to_list(SenderAddr#addr_dto.addr)}
	end, MessagesDTO),
	?log_debug("Retrieved messages in EOneAPI format: ~p", [Messages]),
	{ok, Messages, Total}.

handle_inbound_subscribe(Req, #state{creds = Creds, customer = Customer}) ->
	?log_debug("Got inbound subscribe event: ~p", [Req]),
	#k1api_auth_response_dto{
		uuid = CustomerID
		} = Customer,
	#credentials{user = UserID} = Creds,
	#subscribe_inbound{
		destination_address = DestAddr,
		notify_url = NotifyURL,
		criteria = Criteria, % opt
		callback_data = CallbackData, % opt
		client_correlator = Correlator % opt
		} = Req,
	ReqID = uuid:newid(),
	DTO = #k1api_subscribe_incoming_sms_request_dto{
		id = ReqID,
		customer_id = CustomerID,
		user_id = UserID,
		dest_addr = #addr_dto{addr = convert_addr(DestAddr), ton = 1, npi = 1},
		notify_url = NotifyURL,
		criteria = Criteria,
		correlator = Correlator,
		callback_data = CallbackData
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, SubscriptionID} = k1api_subscription_srv:subscribe_incoming_sms(ReqID, Bin),
	?log_debug("Got subscriptionID: ~p", [SubscriptionID]),
	{ok, uuid:to_string(SubscriptionID)}.

handle_inbound_unsubscribe(SubscribeIDBitstring, State = #state{}) ->
	?log_debug("Got inbound unsubscribe event: ~p", [SubscribeIDBitstring]),
	#state{
		creds = Creds,
		customer = Customer
	} = State,
	#k1api_auth_response_dto{
		uuid = CustomerID
	} = Customer,
	#credentials{user = UserID} = Creds,
	SubscribeID = uuid:to_binary(binary_to_list(SubscribeIDBitstring)),
	RequestID = uuid:newid(),
	DTO = #k1api_unsubscribe_incoming_sms_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = UserID,
		subscription_id = SubscribeID
	},
	?log_debug("Send unsubscribe event: ~p", [DTO]),
	{ok, Bin} = adto:encode(DTO),
	{ok, RequestID} = k1api_subscription_srv:unsubscribe_incoming_sms(RequestID, Bin),
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


convert_addr(<<"tel:+", Bin/binary>>) ->
	Bin;
convert_addr(Bin) when is_binary(Bin) ->
	Bin.
