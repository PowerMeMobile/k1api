-ifndef(eoneapi_hrl).
-define(eoneapi_hrl, included).

-type state() :: term().
-type error() :: term().
-type date() :: {integer(), integer(), integer()}.
-type time() :: {integer(), integer(), integer()}.
-type datetime() :: {date(), time()}.
-type sender_address() :: string().
-type request_id() :: string().
-type address() :: string().
-type delivery_status() :: string().
% 							"DeliveredToTerminal" |
% 							"DeliveryUncertain" |
% 							"DeliveryImpossible" |
% 							"MessageWaiting" | 
% 							"DeliveredToNetwork" .

-record(credentials, {
	system_id :: string(),
	user :: string(),
	password :: string(),
	type :: transmitter | dlvrReceiptReceiver | incomingSMSReceiver
	}).

-type credentials() :: #credentials{}.

-record(outbound_sms, {
	address :: [string()],
	sender_address :: sender_address(),
	message :: string(),
	sender_name :: undefined | string(), %opt
	notify_url :: undefined | string(), %% opt
	client_correlator :: undefined | string(), %opt
	callback_data :: undefined | string() % opt
	}).

-type outbound_sms() :: #outbound_sms{}.


-type sms_delivery_statuses() :: [{address(), delivery_status()}].

-record(delivery_receipt, {
	notify_url :: string(),
	callback_data :: string(),
	address :: string(),
	delivery_status :: delivery_status()
	}).
-type delivery_receipt() :: #delivery_receipt{}.
-record(del_rec_subscribe, {
	sender_address :: sender_address(),
	notify_url :: string(),
	client_correlator :: undefined | string(), % opt
	criteria :: undefined | string(), % opt
	callback_data  :: undefined | string() % opt
	}).
-type del_rec_subscribe() :: #del_rec_subscribe{}.
-type subscription_id() :: string().

-record(retrieve_sms_req, {
	registration_id :: string(),
	batch_size :: integer()
	}).
-type retrieve_sms_req() :: #retrieve_sms_req{}.

%% message notification
-record(inbound_sms, {
	notify_url :: string(),
	date_time :: datetime(),
	destination_address :: string(),
	message_id :: string(),
	message :: string(),
	sender_address :: string(),
	callback_data :: undefined | string()
	}).
-type inbound_sms() :: #inbound_sms{}.
-type pending_sms() :: integer().

-record(subscribe_inbound, {
	destination_address :: string(),
	notify_url :: string(),
	criteria :: undefined | string(), % opt
	callback_data :: undefined | string(), % opt
	client_correlator :: undefined | string() % opt
	}).
-type subscribe_inbound() :: #subscribe_inbound{}.

-endif. % eoneapi_hrl