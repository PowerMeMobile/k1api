-module(k1api_common_test).
-compile(export_all).

%% @TODO Implement test for criteria, correlator, exceptions & also
%% check all json keys for presents & and its' values

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(join(Args), lists:concat(Args)).
-define(url_encode(Args),
	apply(fun(RawList) ->
		KeyValueList = lists:map(fun({K, V}) ->
			K ++ "=" ++ V
		end, RawList),
		Concat = fun
					(KV, "") -> KV;
					(KV, Acc) -> KV ++ "&" ++ Acc
				end,
		lists:foldr(Concat, "", KeyValueList)
	end, [Args])).

-define(json_value(Key, Response),
	apply(fun
			(__Key, __JsonStructBin) when is_binary(__JsonStructBin) ->
 		__JsonStruct = etest_http_json:decode(__JsonStructBin),
	    case etest_http_json:fetch(__Key, __JsonStruct, undefined) of
	        undefined -> erlang:error({json_val_undefined,
			                    [{json_struct, __JsonStruct},
								 {module,   ?MODULE},
          					     {line,     ?LINE}] });
	        Value -> Value
	    end;

			(__Key, __Res) ->
 		__JsonStruct = etest_http_json:decode(__Res#etest_http_res.body),
	    case etest_http_json:fetch(__Key, __JsonStruct, undefined) of
	        undefined -> erlang:error({json_val_undefined,
			                    [{module,   ?MODULE},
          					     {line,     ?LINE}] });
	        Value -> Value
	    end
	end, [Key, Response])).

%% ===================================================================
%% Helpers
%% ===================================================================

prot() -> "http://".
host() -> "127.0.0.1".
port() -> "8081".
postpaid_customer() -> "oneapi-postpaid".
prepaid_customer() -> "oneapi-prepaid".
user() -> "user".
pass() -> "password".
delimiter() -> "@".
sender_addr() -> "tel%3A%2B375296660003".
prepaid_sender_addr() -> "tel%3A%2B375296660004".
k1api_client_msisdn() -> "tel%3A%2B375296660003".
message() -> "Hello World!".
ver() -> "2".
creds() ->
	Creds = ?join([postpaid_customer(), delimiter(), user(), ":", pass()]),
	base64:encode_to_string(Creds).
prepaid_creds() ->
	Creds = ?join([prepaid_customer(), delimiter(), user(), ":", pass()]),
	base64:encode_to_string(Creds).
correlator() -> base64:encode_to_string(crypto:rand_bytes(14)).
addr_preffix() -> ?join([prot(), host(), ":", port(), "/", ver()]).

%% notify urls
notify_port() -> "44444".
sms_notify_url() ->
	?join(["http://127.0.0.1:", notify_port(), "/incoming_sms"]).
receipts_notify_url() ->
	?join(["http://127.0.0.1:", notify_port(), "/receipts"]).



%% ===================================================================
%% Tests
%% ===================================================================

setup() ->
	oneapi_incoming_srv:start().

%% mt_prepaid_sms_test_() ->
%% 	{setup,
%% 		fun setup/0,
%% 		{timeout, 60,
%% 			[?_test(outbound_sms_prepaid())]}}.

outbound_sms_prepaid() ->
    %%
	%% Send sms request with delivery receipts
    %%
	Url = ?join([addr_preffix(), "/smsmessaging/outbound/", sender_addr(), "/requests"]),
	NotifyURL = receipts_notify_url(),
	Body = ?url_encode([{"address", "tel%3A%2B375291234567"},
						{"address", "tel%3A%2B375291234568"},
						{"senderAddress", prepaid_sender_addr()},
						{"message", message()},
						{"clientCorrelator", correlator()},
						{"notifyURL", NotifyURL},
						{"callbackData", "some-data-useful-to-the-requester"},
						{"senderName", "ACME%20Inc."}]),
	Headers = [{"Authorization", prepaid_creds()}],
    Response = ?perform_post(Url, Headers, Body),
    ?assert_status(201, Response),
	?assert_json_key([<<"resourceReference">>, <<"resourceURL">>], Response),

    %%
	%% Send sms status request
    %%
	ResourceUrl = ?json_value([<<"resourceReference">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	SmsStUrl = ?join([addr_preffix(), "/smsmessaging/outbound/", prepaid_sender_addr(), "/requests/", ResourceID, "/deliveryInfos"]),
	SmsStResponse = ?perform_get(SmsStUrl, Headers),
	?assert_status(200, SmsStResponse),
	?assert_json_key([<<"deliveryInfoList">>, <<"deliveryInfo">>], SmsStResponse),
	?assert_json_value([<<"deliveryInfoList">>, <<"resourceURL">>], list_to_binary(SmsStUrl), SmsStResponse),

    %%
	%% Retrive receipts from test http server
    %%
	timer:sleep(5000),
	{ok, GotReceipts} = oneapi_incoming_srv:give_receipts(),
	?assertEqual(2, length(GotReceipts)).

outbound_sms_postpaid_test_() ->
	{setup,
		fun setup/0,
		{timeout, 60,
			[?_test(outbound_sms_postpaid())]}}.

outbound_sms_postpaid() ->
    %%
	%% Send sms request with delivery receipts
    %%
	Url = ?join([addr_preffix(), "/smsmessaging/outbound/", sender_addr(), "/requests"]),
	NotifyURL = receipts_notify_url(),
	Body = ?url_encode([{"address", "tel%3A%2B375251234567"},
						{"address", "tel%3A%2B375251234568"},
						{"senderAddress", sender_addr()},
						{"message", message()},
						{"clientCorrelator", correlator()},
						{"notifyURL", NotifyURL},
						{"callbackData", "some-data-useful-to-the-requester"},
						{"senderName", "ACME%20Inc."}]),
	Headers = [{"Authorization", creds()}],
    Response = ?perform_post(Url, Headers, Body),
    ?assert_status(201, Response),
	?assert_json_key([<<"resourceReference">>, <<"resourceURL">>], Response),

    %%
	%% Send sms status request
    %%
	ResourceUrl = ?json_value([<<"resourceReference">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	SmsStUrl = ?join([addr_preffix(), "/smsmessaging/outbound/", sender_addr(), "/requests/", ResourceID, "/deliveryInfos"]),
	SmsStResponse = ?perform_get(SmsStUrl, Headers),
	?assert_status(200, SmsStResponse),
	?assert_json_key([<<"deliveryInfoList">>, <<"deliveryInfo">>], SmsStResponse),
	?assert_json_value([<<"deliveryInfoList">>, <<"resourceURL">>], list_to_binary(SmsStUrl), SmsStResponse),

    %%
	%% Retrieve receipts from test http server
    %%
	timer:sleep(5000),
	{ok, GotReceipts} = oneapi_incoming_srv:give_receipts(),
	?assertEqual(2, length(GotReceipts)).

%% add send sms & then retrieve sms again
retrieve_sms_test() ->
	Url = ?join([addr_preffix(), "/smsmessaging/inbound/registrations/", sender_addr(), "/messages"]),
	Headers = [{"Authorization", creds()}],
	Queries = [{"maxBatchSize", "2"}],
	Resp = ?perform_get(Url, Headers, Queries),
	?assert_json_key([<<"inboundSMSMessageList">>, <<"inboundSMSMessage">>], Resp),
	?assert_json_key([<<"inboundSMSMessageList">>, <<"numberOfMessagesInThisBatch">>], Resp),
	?assert_json_key([<<"inboundSMSMessageList">>, <<"resourceURL">>], Resp),
	?assert_json_key([<<"inboundSMSMessageList">>, <<"totalNumberOfPendingMessages">>], Resp).

incoming_sms_sub_test_() ->
	{setup,
		fun setup/0,
		{timeout, 60,
			[?_test(incoming_sms_sub())]}}.

incoming_sms_sub() ->
    %%
	%% Subscribe
    %%
	CallbackData = "doSomething()",
	Url = ?join([addr_preffix(), "/smsmessaging/inbound/subscriptions"]),
	Body = ?url_encode([
						{"destinationAddress", k1api_client_msisdn()},
						{"notifyURL", sms_notify_url()},
						%% {"criteria", "TAG"},
						{"notificationFormat", "JSON"},
						{"callbackData", CallbackData},
						{"clientCorrelator", correlator()}
						]),
	Headers = [{"Authorization", creds()}],
    Response = ?perform_post(Url, Headers, Body),
    ?assert_status(201, Response),

    %%
	%% Send incoming sms via SmppSim
    %%
	SendInSmsUrl = "http://localhost:8071/inject_mo",
	TestIncomingSmsText = "TestOneApiIncomingSms",
	Queries = [{"short_message", TestIncomingSmsText},
				{"source_addr", "375441254746"},
				{"destination_addr", "375296660003"},
				{"source_addr_ton", "1"},
				{"source_addr_npi", "0"},
				{"dest_addr_ton", "1"},
				{"dest_addr_npi", "1"}],
	SendInSmsResponse = ?perform_get(SendInSmsUrl, [], Queries),
	?assert_status(200, SendInSmsResponse),

    %%
	%% Receive incoming sms
    %%
	timer:sleep(5000), %% wait for delivery
	{ok, IncomingSMSes} = oneapi_incoming_srv:give_sms(),
	?assertEqual(1, length(IncomingSMSes)),
	[IncomingSMS] = IncomingSMSes,
	DebugCallbackData = ?json_value([<<"inboundSMSMessageNotification">>, <<"callbackData">>], IncomingSMS),
	?assertEqual(DebugCallbackData, list_to_binary(CallbackData)),
	DebugMessageText = ?json_value([<<"inboundSMSMessageNotification">>, <<"inboundSMSMessage">>, <<"message">>], IncomingSMS),
	?assertEqual(DebugMessageText, list_to_binary(TestIncomingSmsText)),
	?json_value([<<"inboundSMSMessageNotification">>,
				<<"inboundSMSMessage">>,
				<<"dateTime">>], IncomingSMS),
	?json_value([<<"inboundSMSMessageNotification">>,
				<<"inboundSMSMessage">>,
				<<"destinationAddress">>], IncomingSMS),
	?json_value([<<"inboundSMSMessageNotification">>,
				<<"inboundSMSMessage">>,
				<<"messageId">>], IncomingSMS),
	?json_value([<<"inboundSMSMessageNotification">>,
				<<"inboundSMSMessage">>,
				<<"senderAddress">>], IncomingSMS),

    %%
	%% Unsubscribe
    %%
	ResourceUrl = ?json_value([<<"resourceReference">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	DelSubUrl = ?join([addr_preffix(), "/smsmessaging/inbound/subscriptions/", ResourceID]),
	DelResponse = ?perform_delete(DelSubUrl, Headers),
	?assert_status(204, DelResponse).

sms_receipts_sub_test() ->
    %%
	%% Subscribe
    %%
	Url = ?join([addr_preffix(), "/smsmessaging/outbound/", k1api_client_msisdn(), "/subscriptions"]),
	NotifyURL = receipts_notify_url(),
	Criteria = "Tag",
	CallbackData = "doSomething()",
	Correlator = correlator(),
	Body = ?url_encode([
						{"clientCorrelator", Correlator},
						{"notifyURL", NotifyURL},
						{"criteria", Criteria},
						{"callbackData", CallbackData}
						]),
	Headers = [{"Authorization", creds()}],
    Response = ?perform_post(Url, Headers, Body),
    ?assert_status(201, Response),
	?assert_json_value([<<"deliveryReceiptSubscription">>, <<"callbackReference">>, <<"callbackData">>],
		list_to_binary(CallbackData), Response),
	?assert_json_value([<<"deliveryReceiptSubscription">>, <<"callbackReference">>, <<"notifyURL">>],
		list_to_binary(NotifyURL), Response),
	?assert_json_value([<<"deliveryReceiptSubscription">>, <<"callbackReference">>, <<"criteria">>],
		list_to_binary(Criteria), Response),

    %%
	%% Unsubscribe
    %%
	ResourceUrl = ?json_value([<<"deliveryReceiptSubscription">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	DelSubUrl = ?join([addr_preffix(), "/smsmessaging/outbound/", k1api_client_msisdn(), "/subscriptions/", ResourceID]),
	DelResponse = ?perform_delete(DelSubUrl, Headers),
	?assert_status(204, DelResponse).
