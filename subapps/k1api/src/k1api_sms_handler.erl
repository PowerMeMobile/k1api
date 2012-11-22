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

-define(record_info(RecordName, Record),
	apply(
	fun() ->
		Fields = record_info(fields, RecordName),
		[_ | Values] = tuple_to_list(Record),
		lists:zip(Fields, Values)
	end, [])).

%% ===================================================================
%% eoneapi sms handler callbacks
%% ===================================================================

init(Creds = #credentials{}) ->
	?log_debug("Credentials: ~p", [Creds]),
	case k1api_auth_srv:authenticate(Creds) of
		{ok, Customer = #k1api_auth_response_dto{}} ->
			?log_debug("Customer: ~p", [Customer]),
			{ok, #state{creds = Creds, customer = Customer}}
	   	%% {error, Error} ->
		%% 	?log_error("~p", [Error]),
		%% 	{error, Error}
	end.

handle_send_sms_req(OutboundSms = #outbound_sms{},
						#state{customer = Customer, creds = Creds}) ->
	?log_debug("Got outbound sms request:  ~p", [OutboundSms]),
	case k1api_outbound_sms_srv:send(OutboundSms, Customer, Creds) of
		{ok, RequestID} ->
			?log_debug("Message sucessfully sent [id: ~p]", [RequestID]),
			{ok, RequestID}
		%% {error, Error} ->
		%% 	?log_debug("Send message error: ~p", [Error]),
		%% 	{error, Error}
	end.

handle_delivery_status_req(SenderAddress, SendSmsRequestID,
						#state{creds = Creds, customer = Customer}) ->
	#k1api_auth_response_dto{
		uuid = CustomerUUID
	} = Customer,
	#credentials{user_id = UserID} = Creds,
	?log_debug("Got delivery status request "
		"[customer: ~p, user: ~p, sender_address: ~p, send_sms_req_id: ~p]",
		[CustomerUUID, UserID, SenderAddress, SendSmsRequestID]),
	{ok, Statuses} = k1api_delivery_status_srv:get(CustomerUUID, UserID, SenderAddress, SendSmsRequestID),

	%% convert [#k1api_sms_status_dto{}] to [{"dest_addr", "status"}]
	DeliveryStatuses = convert_delivery_statuses(Statuses),

	{ok, DeliveryStatuses}.

handle_delivery_notifications_subscribe(Req, State = #state{}) ->
	ReqInfo = ?record_info(delivery_receipt_subscribe, Req),
	?log_debug("Got delivery notifications subscribe request: ~p", [ReqInfo]),
	#state{creds = Creds, customer = Customer} = State,
	#delivery_receipt_subscribe{
		sender_addr = Sender,
		notify_url = Url,
		correlator = Correlator,
		criteria = _Criteria,
		callback = Callback
	} = Req,
	#k1api_auth_response_dto{
		uuid = CustomerUUID
		} = Customer,
	#credentials{user_id = UserID} = Creds,
	ReqID = uuid:newid(),
	case k1api_db:check_correlator(CustomerUUID, UserID, Correlator, ReqID) of
		ok ->
			?log_debug("Correlator saved", []),
			ReqDTO = #k1api_subscribe_sms_receipts_request_dto{
				id = ReqID,
				customer_id = CustomerUUID,
				user_id = UserID,
				url = Url,
				dest_addr = #addr_dto{addr = Sender, ton = 1, npi = 1},
				callback_data = Callback
			},
			?log_debug("ReqDTO: ~p", [ReqDTO]),
			{ok, Bin} = adto:encode(ReqDTO),
			{ok, _RespBin} = k1api_subscription_srv:subscribe_receipts(ReqID, Bin),
			{ok, ReqID};
		{correlator_exist, OrigReqID} ->
			?log_debug("Correlator exist: ~p", [OrigReqID]),
			{ok, OrigReqID}
	end.

handle_delivery_notifications_unsubscribe(_SenderAdress, SubscriptionID, State = #state{}) ->
	#state{creds = Creds, customer = Customer} = State,
	#credentials{user_id = UserID} = Creds,
	#k1api_auth_response_dto{
		uuid = CustomerID
		} = Customer,
	RequestID = uuid:newid(),
	DTO = #k1api_unsubscribe_sms_receipts_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = UserID,
		subscription_id = SubscriptionID
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, ok} = k1api_subscription_srv:unsubscribe_receipts(RequestID, Bin),
	?log_debug("Subscription [id: ~p] was successfully removed", [SubscriptionID]),
	{ok, deleted}.

handle_retrieve_req(Request = #retrieve_sms_req{}, State = #state{}) ->
	#retrieve_sms_req{
		reg_id = RegID,
		batch_size = BatchSize
	} = Request,
	#state{creds = Creds, customer = Customer} = State,
	#k1api_auth_response_dto{
		uuid = CustomerUUID
	} = Customer,
	#credentials{user_id = UserID} = Creds,
	?log_debug("Sending retrieve sms request", []),
	DestinationAddress = RegID,
	{ok, Response} =
		k1api_retrieve_sms_srv:get(CustomerUUID, UserID, DestinationAddress, BatchSize),
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
			message_id = MessageID,
			message = MessageText,
			sender_addr = SenderAddr#addr_dto.addr}
	end, MessagesDTO),
	?log_debug("Retrieved messages in EOneAPI format: ~p", [Messages]),
	{ok, Messages, Total}.

handle_inbound_subscribe(Req, #state{creds = Creds, customer = Customer}) ->
	?log_debug("Got inbound subscribe event: ~p", [Req]),
	#k1api_auth_response_dto{
		uuid = CustomerID
		} = Customer,
	#credentials{user_id = UserID} = Creds,
	#subscribe_inbound{
		dest_addr = DestAddr,
		notify_url = NotifyURL,
		criteria = Criteria, % opt
		callback = CallbackData, % opt
		correlator = Correlator % opt
	} = Req,
	ReqID = uuid:newid(),
	?log_debug("Got correlator: ~p", [Correlator]),
	case k1api_db:check_correlator(CustomerID, UserID, Correlator, ReqID) of
		ok ->
			?log_debug("Correlator saved", []),
			DTO = #k1api_subscribe_incoming_sms_request_dto{
				id = ReqID,
				customer_id = CustomerID,
				user_id = UserID,
				dest_addr = #addr_dto{addr = DestAddr, ton = 1, npi = 1},
				notify_url = NotifyURL,
				criteria = Criteria,
				correlator = Correlator,
				callback_data = CallbackData
			},
			{ok, Bin} = adto:encode(DTO),
			{ok, SubscriptionID} = k1api_subscription_srv:subscribe_incoming_sms(ReqID, Bin),
			?log_debug("Got subscriptionID: ~p", [SubscriptionID]),
			{ok, SubscriptionID};
		{correlator_exist, OrigReqID} ->
			?log_debug("Correlator exist: ~p", [OrigReqID]),
			{ok, OrigReqID}
	end.

handle_inbound_unsubscribe(SubscribeID, State = #state{}) ->
	?log_debug("Got inbound unsubscribe event: ~p", [SubscribeID]),
	#state{
		creds = Creds,
		customer = Customer
	} = State,
	#k1api_auth_response_dto{
		uuid = CustomerID
	} = Customer,
	#credentials{user_id = UserID} = Creds,
	RequestID = uuid:newid(),
	DTO = #k1api_unsubscribe_incoming_sms_request_dto{
		id = RequestID,
		customer_id = CustomerID,
		user_id = UserID,
		subscription_id = SubscribeID
	},
	?log_debug("Send unsubscribe event: ~p", [DTO]),
	{ok, Bin} = adto:encode(DTO),
	{ok, ok} = k1api_subscription_srv:unsubscribe_incoming_sms(RequestID, Bin),
	{ok, deleted}.

%% ===================================================================
%% Internal
%% ===================================================================

convert_delivery_statuses(Status = #k1api_sms_status_dto{}) ->
	#k1api_sms_status_dto{
		address = AddrDTO,
		status = StatusNameDTO
	} = Status,
	{AddrDTO#addr_dto.addr, translate_status_name(StatusNameDTO)};
convert_delivery_statuses(Statuses) ->
	[convert_delivery_statuses(Status) || Status <- Statuses].

translate_status_name(submitted) ->
	<<"MessageWaiting">>;
translate_status_name(success_waiting_delivery) ->
	<<"MessageWaiting">>;
translate_status_name(success_no_delivery) ->
	<<"DeliveryImpossible">>;
translate_status_name(failure) ->
	<<"DeliveryUncertain">>;
translate_status_name(enroute) ->
	<<"Enroute">>;
translate_status_name(delivered) ->
	<<"DeliveredToTerminal">>;
translate_status_name(expired) ->
	<<"DeliveryImpossible">>;
translate_status_name(deleted) ->
	<<"Deleted">>;
translate_status_name(undeliverable) ->
	<<"DeliveryImpossible">>;
translate_status_name(accepted) ->
	<<"DeliveredToNetwork">>;
translate_status_name(unknown) ->
	<<"DeliveryUncertain">>;
translate_status_name(rejected) ->
	<<"Rejected">>;
translate_status_name(unrecognized) ->
	<<"Unrecognized">>.
