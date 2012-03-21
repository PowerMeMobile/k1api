-module(eoneapi).
-compile([{parse_transform, lager_transform}]).
-include("logging.hrl").
-include("eoneapi.hrl").
-define(VERSION, <<"2">>).
-export([
	start_service/1,
	get_spec/1,
	deliver_sms_status/2,
	deliver_sms/2,
	code/3
	]).

% -export([
% 	test/0
% 	]).
%%%%%%%%%%%%%%%%
%% API functions
%%%%%%%%%%%%%%%%

-spec start_service([{term(), term()}]) -> ignore. 
start_service(EOneAPIProps) ->
	Port = proplists:get_value(port, EOneAPIProps, 8080),
	SmsHanlerSpec = build_sms_handle_spec(EOneAPIProps),
	Dispatch = [
		{'_', SmsHanlerSpec ++ 
			[
			{'_', eoa_default_handler, []}
			]}
			],
	cowboy:start_listener(my_http_listener, 1,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
		).

-spec get_spec([{term(), term()}]) -> ignore.
get_spec(EOneAPIProps) ->
	build_sms_handle_spec(EOneAPIProps).

-spec deliver_sms_status(term(), delivery_receipt()) ->
	{ok, success} |
	{error, term()}. 
deliver_sms_status(_NotificationFormat, #delivery_receipt{
														notify_url = NotifyURL,
														callback_data = CallbackData,
														address = Address,
														delivery_status = DeliveryStatus
														}) ->
	ContentType = "application/json",
	AddrBin = list_to_binary(Address),
	DeliveryStatusBin = list_to_binary(DeliveryStatus),
	Body =
		<<
			<<"{\"deliveryInfoNotification\":{\"callbackData\":\"">>/binary,
			CallbackData/binary,
			<<"\",\"deliveryInfo\":{\"address\":\"tel:+">>/binary,
			AddrBin/binary,
			<<"\",\"deliveryStatus\":\"">>/binary,
			DeliveryStatusBin/binary,
			<<"\"}}}">>/binary
		>>,
	Response =
		httpc:request(post, {NotifyURL, [], ContentType, Body}, [{timeout, 5000}], [{body_format, binary}]),
	case Response of
		{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
			{ok, success};
		Error -> {error, Error}
	end.

% test() ->
% 	NotifyURL = "http://127.0.0.1:8080/youUrlHere",
% 	% deliver_sms_status(undef, #delivery_receipt{
% 	% 								notify_url = NotifyURL,
% 	% 								callback_data = <<"CallBack">>,
% 	% 								address = "1234567890",
% 	% 								delivery_status = "Status"
% 	% 							}).
% 	deliver_sms(undef, #inbound_sms{
% 									notify_url = NotifyURL,
% 									date_time = {{2012,10,24}, {13,45,00}},
% 									destination_address = "293761099",
% 									message_id = "mes123",
% 									message = "hello world",
% 									sender_address = "443458320",
% 									callback_data = <<"some callback data">>
% 		}).

-spec deliver_sms(term(), inbound_sms()) -> ok.
deliver_sms(_NotificationFormat, #inbound_sms{
									notify_url = NotifyURL,
									date_time = DateTime,
									destination_address = DestAddr,
									message_id = MessId,
									message = Message,
									sender_address = SenderAddr,
									callback_data = CallBack
									}) ->
	DateTimeBin = iso8601:format(DateTime),
	DestAddrBin = list_to_binary(DestAddr),
	MessIdBin = list_to_binary(MessId),
	MessageBin = list_to_binary(Message),
	SenderAddrBin = list_to_binary(SenderAddr),
	Body = 
		<<
			<<"{\"inboundSMSMessageNotification\":{\"callbackData\":\"">>/binary,
			CallBack/binary,
			<<"\",\"inboundSMSMessage\":{\"dateTime\":\"">>/binary,
			DateTimeBin/binary,
			<<"\",\"destinationAddress\":\"">>/binary,
			DestAddrBin/binary,
			<<"\",\"messageId\":\"">>/binary,
			MessIdBin/binary,
			<<"\",\"message\":\"">>/binary,
			MessageBin/binary,
			<<"\",\"senderAddress\":\"+">>/binary,
			SenderAddrBin/binary,
			<<"\"}}}">>/binary
		>>,
	ContentType = "application/json",
	Response =
		httpc:request(post, {NotifyURL, [], ContentType, Body}, [{timeout, 5000}], [{body_format, binary}]),
	case Response of
		{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
			{ok, success};
		Error -> {error, Error}
	end.




	%% HTTP codes

-spec code(integer(), term(), term()) -> {ok, term(), term()}.
code(500, Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(500, [],
		<<"Internal Server Error">>, Req),
	{ok, Req2, State};

code(401, Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(401, [{'Www-Authenticate', <<"Basic">>}],
										<<"Authentication failure, check your authentication details">>, Req),
	{ok, Req2, State};

code(404, Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(404, [], <<"Not found: mistake in the host or path of the service URI">>, Req),
	{ok, Req2, State}.

%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%

build_sms_handle_spec(EOneAPIProps) ->

	case proplists:get_value(sms_handler, EOneAPIProps, undefined) of
		undefined ->
			[];
		SmsHandler ->
			[
				{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"requests">>], eoa_sms_handler, [SmsHandler]},
				{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"requests">>, '_', <<"deliveryInfos">>], eoa_sms_handler, [SmsHandler]},
				{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"subscriptions">>], eoa_sms_handler, [SmsHandler]},
				{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"subscriptions">>, '_'], eoa_sms_handler, [SmsHandler]},
				{[?VERSION, <<"smsmessaging">>, <<"inbound">>, <<"registrations">>, '_', <<"messages">>], eoa_sms_handler, [SmsHandler]},
				{[?VERSION, <<"smsmessaging">>, <<"inbound">>, <<"subscriptions">>], eoa_sms_handler, [SmsHandler]},
				{[?VERSION, <<"smsmessaging">>, <<"inbound">>, <<"subscriptions">>, '_'], eoa_sms_handler, [SmsHandler]}
			]
	end.