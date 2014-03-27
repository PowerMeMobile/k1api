-module(eoneapi).

-include("eoneapi.hrl").

-define(VERSION, <<"2">>).

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
	Port = proplists:get_value(port, EOneAPIProps, 8080),
	SmsHanlerSpec = build_sms_handle_spec(EOneAPIProps),
	Dispatch =
		[{'_', SmsHanlerSpec ++
			[{'_', eoa_error_handler, []}]}],
	cowboy:start_listener(my_http_listener, 1,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Dispatch}]).

-spec build_sms_handle_spec([{any(), any()}]) -> any().
build_sms_handle_spec(EOneAPIProps) ->
	case proplists:get_value(sms_handler, EOneAPIProps, undefined) of
		undefined ->
			[];
		SmsHandler ->
			[{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"requests">>],
				eoa_sms_handler, [SmsHandler]},
			{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"requests">>, '_', <<"deliveryInfos">>],
				eoa_sms_handler, [SmsHandler]},
			{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"subscriptions">>],
				eoa_sms_handler, [SmsHandler]},
			{[?VERSION, <<"smsmessaging">>, <<"outbound">>, '_', <<"subscriptions">>, '_'],
				eoa_sms_handler, [SmsHandler]},
			{[?VERSION, <<"smsmessaging">>, <<"inbound">>, <<"registrations">>, '_', <<"messages">>],
				eoa_sms_handler, [SmsHandler]},
			{[?VERSION, <<"smsmessaging">>, <<"inbound">>, <<"subscriptions">>],
				eoa_sms_handler, [SmsHandler]},
			{[?VERSION, <<"smsmessaging">>, <<"inbound">>, <<"subscriptions">>, '_'],
				eoa_sms_handler, [SmsHandler]}]
	end.

-spec deliver_sms_status(delivery_receipt()) ->	{ok, term()} | {error, term()}.
deliver_sms_status(#delivery_receipt{	notify_url = NotifyURL,
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
	httpc:request(	post,
					{binary_to_list(NotifyURL),	[],	ContentType, JsonBody},
					[{timeout, 5000}],
					[{body_format, binary}]	).

-spec deliver_sms(inbound_sms()) -> {ok, term()} | {error, term()}.
deliver_sms(#inbound_sms{
							notify_url = NotifyURL,
							date_time = DateTime,
							dest_addr = DestAddr,
							message_id = MessId,
							message = Message,
							sender_addr = SenderAddr,
							callback = CallBack		}) ->
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
	httpc:request(	post,
					{binary_to_list(NotifyURL), [], ContentType, JsonBody},
					[{timeout, 5000}],
					[{body_format, binary}]).

%% ===================================================================
%% HTTP Response Codes
%% ===================================================================

-spec code(integer(), term(), term()) -> {ok, term(), term()}.
code(500, Req, State) ->
	Body = <<"Internal Server Error">>,
	{ok, Req2} = cowboy_http_req:reply(500, [], Body, Req),
	{ok, Req2, State};

code(401, Req, State) ->
	Headers = [{'Www-Authenticate', <<"Basic">>}],
	Body = <<"Authentication failure, check your authentication details">>,
	{ok, Req2} = cowboy_http_req:reply(401, Headers, Body, Req),
	{ok, Req2, State};

code(404, Req, State) ->
	Body = <<"Not found: mistake in the host or path of the service URI">>,
	{ok, Req2} = cowboy_http_req:reply(404, [], Body, Req),
	{ok, Req2, State}.

%% ===================================================================
%% Exceptions
%% ===================================================================

-spec exception(ExceptionTag :: atom(), Req :: term(), State :: term(), Variables :: [term()]) ->
	{ok, Req2 :: term(), State :: term()}.
exception(ExceptionTag, Variables, Req, State) ->
	{ok, Body, Code} = exception_body_and_code(ExceptionTag, Variables),
	Headers = [{'Content-Type', <<"application/json">>}],
	{ok, Req2} = cowboy_http_req:reply(Code, Headers, Body, Req),
	{ok, Req2, State}.

%% SMS service exceptions

exception_body_and_code('svc0280', Variables) ->
	MessageID = <<"SVC0280">>,
	Text = <<"Message too long. Maximum length is %1 characters.">>,
	1 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0283', Variables) ->
	MessageID = <<"SVC0283">>,
	Text = <<"Delivery receipt notification not supported.">>,
	Variables = [],
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

%% Common Service Exceptions (http://oneapi.gsmworld.com/common-service-exceptions/)

exception_body_and_code('svc0001', Variables) ->
	MessageID = <<"SVC0001">>,
	% %1 – explanation of the error
	Text = <<"A service error occurred. Error code is %1">>,
	1 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0002', Variables) ->
	MessageID = <<"SVC0002">>,
	% %1 – the part of the request that is invalid
	Text = <<"Invalid input value for message part %1">>,
	1 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0003', Variables) ->
	MessageID = <<"SVC0003">>,
	% %1 – message part, %2 – list of valid values
	Text = <<"Invalid input value for message part %1, valid values are %2">>,
	2 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0004', Variables) ->
	MessageID = <<"SVC0004">>,
	% %1 – message part.
	Text = <<"No valid addresses provided in message part %1">>,
	1 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400}; %% can be 404 too, but didn't implemented

exception_body_and_code('svc0005', Variables) ->
	MessageID = <<"SVC0005">>,
	% %1 – Correlator, %2 – message part
	Text = <<"Correlator %1 specified in message part %2 is a duplicate">>,
	2 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 409};

exception_body_and_code('svc0006', Variables) ->
	MessageID = <<"SVC0006">>,
	% %1 – identifier for the invalid group, %2 – message part
	Text = <<"Group %1 in message part %2 is not a valid group">>,
	2 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0007', Variables) ->
	MessageID = <<"SVC0007">>,
	Text = <<"Invalid charging information">>,
	0 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc0008', Variables) ->
	MessageID = <<"SVC0008">>,
	% %1 Message Part with the overlapped criteria
	Text = <<"Overlapped criteria %1">>,
   	1 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('svc1000', Variables) ->
	MessageID = <<"SVC1000">>,
	Text = <<"No resources">>,
	0 = length(Variables),
	{ok, Body} = service_exception_body(MessageID, Text, Variables),
	{ok, Body, 503};

%% Common Policy Exceptions

exception_body_and_code('pol0001', Variables) ->
	MessageID = <<"POL0001">>,
	% %1 – explanation of the error
	Text = <<"A policy error occurred. Error code is %1">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0002', Variables) ->
	MessageID = <<"POL0002">>,
	% %1 – address privacy verification failed for
	Text = <<"Privacy verification failed for address %1, request is refused">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0003', Variables) ->
	MessageID = <<"POL0003">>,
	% %1 – message part
	Text = <<"Too many addresses specified in message part %1">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0004', Variables) ->
	MessageID = <<"POL0004">>,
	% none variables
	Text = <<"Unlimited notification request not supported">>,
   	0 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0005', Variables) ->
	MessageID = <<"POL0005">>,
	% none variables
	Text = <<"Unlimited notification request not supported">>,
   	0 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0006', Variables) ->
	MessageID = <<"POL0006">>,
	% %1 – message part.
	% Note: group means an address which refers to more than one end user.
	Text = <<"Group specified in message part %1 not allowed">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0007', Variables) ->
	MessageID = <<"POL0007">>,
	%% %1 – message part
	%% Note: group means an address which refers to more than one
	%% end user. Groups cannot contain addresses which are themselves groups
	Text = <<"Nested groups specified in message part %1 not allowed">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0008', Variables) ->
	MessageID = <<"POL0008">>,
	%% None variables
	Text = <<"Charging is not supported">>,
   	0 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0009', Variables) ->
	MessageID = <<"POL0009">>,
	%% None variables
	Text = <<"Invalid frequency requested">>,
   	0 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0010', Variables) ->
	MessageID = <<"POL0010">>,
	%% None variables
	Text = <<"Requested information unavailable as the retention time interval has expired">>,
   	0 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 404}; % 403 & 410 can use

exception_body_and_code('pol0011', Variables) ->
	MessageID = <<"POL0011">>,
	%% None variables
	Text = <<"Media type not supported">>,
   	0 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403}; % 406 can used

exception_body_and_code('pol0012', Variables) ->
	MessageID = <<"POL0012">>,
	%% %1 – message part
	Text = <<"Too many description entries specified in message part %1">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol0013', Variables) ->
	MessageID = <<"POL0013">>,
	%% %1 – duplicated addresses
	Text = <<"Duplicated addresses %1">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 400};

exception_body_and_code('pol1009', Variables) ->
	MessageID = <<"POL1009">>,
	%% %1 – name of the service
	Text = <<"User has not been provisioned for %1">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403};

exception_body_and_code('pol1010', Variables) ->
	MessageID = <<"POL1010">>,
	%% %1 – the name of the service
	Text = <<"User has been suspended from %1">>,
   	1 = length(Variables),
	{ok, Body} = policy_exception_body(MessageID, Text, Variables),
	{ok, Body, 403}.

policy_exception_body(MessageID, Text, Variables) ->
	exception_body(<<"policyException">>, MessageID, Text, Variables).
service_exception_body(MessageID, Text, Variables) ->
	exception_body(<<"serviceException">>, MessageID, Text, Variables).

exception_body(ExceptionType, MessageID, Text, Variables) ->
	Body = jsx:encode([{<<"requestError">>, [{ ExceptionType, [
													{<<"messageId">>, MessageID},
													{<<"text">>, Text},
													{<<"variables">>, Variables}
													] }] }]),
	{ok, Body}.

%% ===================================================================
%% Internal Functions
%% ===================================================================
