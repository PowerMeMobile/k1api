-ifndef(FRONTENDUPEVENT_PB_H).
-define(FRONTENDUPEVENT_PB_H, true).
-record(frontendupevent, {
    reset_subscriptions = erlang:error({required, reset_subscriptions}),
    auth_q = erlang:error({required, auth_q}),
    register_q = erlang:error({required, register_q}),
    control_q = erlang:error({required, control_q}),
    timestamp
}).
-endif.

-ifndef(FRONTENDDOWNEVENT_PB_H).
-define(FRONTENDDOWNEVENT_PB_H, true).
-record(frontenddownevent, {
    reason,
    timestamp
}).
-endif.

-ifndef(AUTHREQ_PB_H).
-define(AUTHREQ_PB_H, true).
-record(authreq, {
    system_id = erlang:error({required, system_id}),
    user_id = erlang:error({required, user_id}),
    password = erlang:error({required, password}),
    type = erlang:error({required, type}),
    is_cached = erlang:error({required, is_cached}),
    timestamp = erlang:error({required, timestamp})
}).
-endif.

-ifndef(PB_PROVIDER_PB_H).
-define(PB_PROVIDER_PB_H, true).
-record(pb_provider, {
    id = erlang:error({required, id}),
    gateway = erlang:error({required, gateway}),
    bulk_gateway = erlang:error({required, bulk_gateway}),
    receipts_supported = erlang:error({required, receipts_supported})
}).
-endif.

-ifndef(PB_NETWORK_PB_H).
-define(PB_NETWORK_PB_H, true).
-record(pb_network, {
    id = erlang:error({required, id}),
    country_code = erlang:error({required, country_code}),
    numbers_len = erlang:error({required, numbers_len}),
    prefixes = [],
    provider_id = erlang:error({required, provider_id})
}).
-endif.

-ifndef(PB_ADDR_PB_H).
-define(PB_ADDR_PB_H, true).
-record(pb_addr, {
    addr = erlang:error({required, addr}),
    ton = erlang:error({required, ton}),
    npi = erlang:error({required, npi})
}).
-endif.

-ifndef(PB_CUSTOMER_PB_H).
-define(PB_CUSTOMER_PB_H, true).
-record(pb_customer, {
    id = erlang:error({required, id}),
    uuid = erlang:error({required, uuid}),
    priority = erlang:error({required, priority}),
    rps,
    allowed_sources = [],
    default_source,
    networks = [],
    providers = [],
    default_provider_id,
    receipts_allowed = erlang:error({required, receipts_allowed}),
    no_retry = erlang:error({required, no_retry}),
    default_validity = erlang:error({required, default_validity}),
    max_validity = erlang:error({required, max_validity})
}).
-endif.

-ifndef(AUTHRESPONSE_PB_H).
-define(AUTHRESPONSE_PB_H, true).
-record(authresponse, {
    result = erlang:error({required, result}),
    customer,
    error
}).
-endif.

-ifndef(SUBSCRIBEEVENT_PB_H).
-define(SUBSCRIBEEVENT_PB_H, true).
-record(subscribeevent, {
    subscribe_id = erlang:error({required, subscribe_id}),
    queue_name = erlang:error({required, queue_name}),
    customer_id = erlang:error({required, customer_id}),
    user_id = erlang:error({required, user_id}),
    type = erlang:error({required, type}),
    destination_addr,
    notify_url,
    criteria,
    notification_format,
    client_correlator,
    callback_data
}).
-endif.

-ifndef(UNSUBSCRIBEEVENT_PB_H).
-define(UNSUBSCRIBEEVENT_PB_H, true).
-record(unsubscribeevent, {
    subscribe_id = erlang:error({required, subscribe_id}),
    reason = erlang:error({required, reason})
}).
-endif.

-ifndef(OUTGOINGMESSAGE_PB_H).
-define(OUTGOINGMESSAGE_PB_H, true).
-record(outgoingmessage, {
    source = erlang:error({required, source}),
    dest = erlang:error({required, dest}),
    message = erlang:error({required, message}),
    data_coding = erlang:error({required, data_coding})
}).
-endif.

-ifndef(OUTGOINGBATCH_PB_H).
-define(OUTGOINGBATCH_PB_H, true).
-record(outgoingbatch, {
    id = erlang:error({required, id}),
    messages = []
}).
-endif.

