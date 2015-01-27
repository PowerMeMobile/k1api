-ifndef(oneapi_srv_hrl).
-define(oneapi_srv_hrl, included).

-include_lib("alley_dto/include/adto.hrl").

-type state()           :: term().
-type date()            :: {integer(), integer(), integer()}.
-type time()            :: {integer(), integer(), integer()}.
-type datetime()        :: {date(), time()}.
-type request_id()      :: uuid().
-type address()         :: binary().
-type sender_address()  :: address().
-type delivery_status() :: binary().
-type exception()       :: atom().
-type excep_params()    :: [term()].

-record(credentials, {
    customer_id         :: binary(),
    user_id             :: binary(),
    password            :: binary()
}).
-type credentials() :: #credentials{}.

-record(outbound_sms, {
    address             :: [address()],
    sender_address      :: address(),
    message             :: binary(),
    sender_name         :: undefined | binary(),
    notify_url          :: undefined | binary(),
    client_correlator   :: undefined | binary(),
    callback_data       :: undefined | binary()
}).
-type outbound_sms()    :: #outbound_sms{}.

-type sms_delivery_statuses() :: [{address(), delivery_status()}].

-record(delivery_receipt, {
    notify_url          :: binary(),
    callback_data       :: undefined | binary(),
    dest_addr           :: binary(),
    status              :: delivery_status()
}).
-type delivery_receipt() :: #delivery_receipt{}.

-record(subscribe_delivery_notifications, {
    notify_url          :: binary(),
    client_correlator   :: undefined | binary(),
    criteria            :: undefined | binary(),
    callback_data       :: undefined | binary(),
    sender_addr         :: address()
}).
-type subscribe_delivery_notifications() :: #subscribe_delivery_notifications{}.
-type subscription_id() :: uuid().

-record(retrieve_sms_req, {
    reg_id              :: binary(),
    batch_size          :: integer()
}).
-type retrieve_sms_req() :: #retrieve_sms_req{}.

-record(inbound_sms, {
    notify_url          :: binary(),
    date_time           :: datetime(),
    dest_addr           :: binary(),
    message_id          :: binary(),
    message             :: binary(),
    sender_addr         :: binary(),
    callback_data       :: undefined | binary()
}).
-type inbound_sms() :: #inbound_sms{}.
-type pending_sms() :: integer().

-record(subscribe_inbound, {
    dest_addr           :: binary(),
    notify_url          :: binary(),
    criteria            :: undefined | binary(),
    callback_data       :: undefined | binary(),
    correlator          :: undefined | binary()
}).
-type subscribe_inbound() :: #subscribe_inbound{}.

-endif. % oneapi_srv_hrl
