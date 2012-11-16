-ifndef(eoneapi_hrl).
-define(eoneapi_hrl, included).

-type uuid() 			:: binary().
-type state() 			:: term().
-type date() 			:: {integer(), integer(), integer()}.
-type time() 			:: {integer(), integer(), integer()}.
-type datetime() 		:: {date(), time()}.
-type request_id() 		:: uuid().
-type address() 		:: bitstring().
-type sender_address()	:: address().
-type delivery_status() :: bitstring().
-type exception() 		:: atom().
-type excep_params()	:: [term()].

-record(credentials, {
	system_id		 	:: bitstring(),
	user_id				:: bitstring(),
	password		 	:: bitstring()
}).
-type credentials() :: #credentials{}.

-record(outbound_sms, {
	dest_addr 			:: [address()],
	sender_addr 		:: address(),
	message 			:: bitstring(),
	sender_name 		:: undefined | bitstring(),
	notify_url 			:: undefined | bitstring(),
	correlator		 	:: undefined | bitstring(),
	callback	 		:: undefined | bitstring()
}).
-type outbound_sms() 	:: #outbound_sms{}.

-type sms_delivery_statuses() :: [{address(), delivery_status()}].

-record(delivery_receipt, {
	notify_url 			:: bitstring(),
	callback		 	:: undefined | bitstring(),
	dest_addr	 		:: bitstring(),
	status				:: delivery_status()
}).
-type delivery_receipt() :: #delivery_receipt{}.

-record(delivery_receipt_subscribe, {
	sender_addr 		:: address(),
	notify_url 			:: bitstring(),
	correlator		 	:: undefined | bitstring(),
	criteria 			:: undefined | bitstring(),
	callback	  		:: undefined | bitstring()
}).
-type delivery_receipt_subscribe() :: #delivery_receipt_subscribe{}.
-type subscription_id() :: uuid().

-record(retrieve_sms_req, {
	reg_id				:: bitstring(),
	batch_size 			:: integer()
}).
-type retrieve_sms_req() :: #retrieve_sms_req{}.

-record(inbound_sms, {
	notify_url 			:: bitstring(),
	date_time 			:: datetime(),
	dest_addr			:: bitstring(),
	message_id 			:: bitstring(),
	message 			:: bitstring(),
	sender_addr 		:: bitstring(),
	callback	 		:: undefined | bitstring()
}).
-type inbound_sms() :: #inbound_sms{}.
-type pending_sms() :: integer().

-record(subscribe_inbound, {
	dest_addr			:: bitstring(),
	notify_url 			:: bitstring(),
	criteria 			:: undefined | bitstring(),
	callback	 		:: undefined | bitstring(),
	correlator 			:: undefined | bitstring()
}).
-type subscribe_inbound() :: #subscribe_inbound{}.

-endif. % eoneapi_hrl
