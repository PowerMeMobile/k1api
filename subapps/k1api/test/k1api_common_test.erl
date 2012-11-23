-module(k1api_common_test).
-compile(export_all).

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
	apply(fun(__Key, __Res) ->
		__JsonStruct = etest_http_json:decode(__Res#etest_http_res.body),
	    case etest_http_json:fetch(__Key, __JsonStruct, undefined) of
	        undefined -> .erlang:error({json_val_undefined,
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
user() -> "user".
pass() -> "password".
delimiter() -> "@".
sender_addr() -> "tel%3A%2B375296660003".
k1api_client_msisdn() -> "tel%3A%2B375296660003".
message() -> "Hello World!".
ver() -> "2".
creds() ->
	Creds = ?join([postpaid_customer(), delimiter(), user(), ":", pass()]),
	base64:encode_to_string(Creds).
correlator() -> "correlator".
addr_preffix() -> ?join([prot(), host(), ":", port(), "/", ver()]).

%% ===================================================================
%% Tests
%% ===================================================================

outbound_sms_postpaid_test() ->

	%% Send sms request

	Url = ?join([addr_preffix(), "/smsmessaging/outbound/", sender_addr(), "/requests"]),
	Body = ?url_encode([{"address", "tel%3A%2B13500000991"},
						{"address", "tel%3A%2B13500000992"},
						{"senderAddress", sender_addr()},
						{"message", message()},
						{"clientCorrelator", correlator()},
						{"notifyURL", "http://application.example.com/notifications/DeliveryInfoNotification"},
						{"callbackData", "some-data-useful-to-the-requester"},
						{"senderName", "ACME%20Inc."}]),
	Headers = [{"Authorization", creds()}],
    Response = ?perform_post(Url, Headers, Body),
    ?assert_status(201, Response),
	?assert_json_key([<<"resourceReference">>, <<"resourceURL">>], Response),

	%% Send sms status request

	ResourceUrl = ?json_value([<<"resourceReference">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	SmsStUrl = ?join([addr_preffix(), "/smsmessaging/outbound/", sender_addr(), "/requests/", ResourceID, "/deliveryInfos"]),
	SmsStResponse = ?perform_get(SmsStUrl, Headers),
	?assert_status(200, SmsStResponse),
	?assert_json_key([<<"deliveryInfoList">>, <<"deliveryInfo">>], SmsStResponse),
	?assert_json_value([<<"deliveryInfoList">>, <<"resourceURL">>], list_to_binary(SmsStUrl), SmsStResponse).

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

incoming_sms_sub_test() ->

	%% Subscribe

	Url = ?join([addr_preffix(), "/smsmessaging/inbound/subscriptions"]),
	Body = ?url_encode([
						{"destinationAddress", k1api_client_msisdn()},
						{"notifyURL", "http://localhost:33333/notifications/DeliveryInfoNotification"},
						{"criteria", "TAG"},
						{"notificationFormat", "JSON"},
						{"callbackData", "doSomething()"},
						{"clientCorrelator", correlator()}
						]),
	Headers = [{"Authorization", creds()}],
    Response = ?perform_post(Url, Headers, Body),
    ?assert_status(201, Response),

	%% Unsubscribe

	ResourceUrl = ?json_value([<<"resourceReference">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	DelSubUrl = ?join([addr_preffix(), "/smsmessaging/inbound/subscriptions/", ResourceID]),
	DelResponse = ?perform_delete(DelSubUrl, Headers),
	?assert_status(204, DelResponse).

sms_receipts_sub_test() ->

	%% Subscribe

	Url = ?join([addr_preffix(), "/smsmessaging/outbound/", k1api_client_msisdn(), "/subscriptions"]),
	NotifyURL = "http://localhost:33333/notifications/DeliveryInfoNotification",
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

	%% Unsubscribe

	ResourceUrl = ?json_value([<<"deliveryReceiptSubscription">>, <<"resourceURL">>], Response),
	ResourceID = binary_to_list(filename:basename(ResourceUrl)),
	DelSubUrl = ?join([addr_preffix(), "/smsmessaging/outbound/", k1api_client_msisdn(), "/subscriptions/", ResourceID]),
	DelResponse = ?perform_delete(DelSubUrl, Headers),
	?assert_status(204, DelResponse).
