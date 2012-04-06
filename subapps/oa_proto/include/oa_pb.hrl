-record(frontendupevent, {
    reset_subscriptions = erlang:error({required, reset_subscriptions}),
    auth_q = erlang:error({required, auth_q}),
    register_q = erlang:error({required, register_q}),
    control_q = erlang:error({required, control_q})
}).

-record(frontenddownevent, {
    reason
}).

-record(authreq, {
    system_id = erlang:error({required, system_id}),
    user_id = erlang:error({required, user_id}),
    password = erlang:error({required, password}),
    type = erlang:error({required, type}),
    is_cached = erlang:error({required, is_cached}),
    timestamp = erlang:error({required, timestamp})
}).

-record(provider, {
    id = erlang:error({required, id}),
    gateway = erlang:error({required, gateway}),
    bulk_gateway = erlang:error({required, bulk_gateway}),
    receipts_supported = erlang:error({required, receipts_supported})
}).

-record(network, {
    id = erlang:error({required, id}),
    country_code = erlang:error({required, country_code}),
    numbers_len = erlang:error({required, numbers_len}),
    prefixes,
    provider_id = erlang:error({required, provider_id})
}).

-record(addr, {
    addr = erlang:error({required, addr}),
    ton = erlang:error({required, ton}),
    npi = erlang:error({required, npi})
}).

-record(customer, {
    id = erlang:error({required, id}),
    uuid = erlang:error({required, uuid}),
    priority = erlang:error({required, priority}),
    rps,
    allowed_sources,
    default_source,
    networks,
    providers,
    default_provider_id,
    receipts_allowed = erlang:error({required, receipts_allowed}),
    no_retry = erlang:error({required, no_retry}),
    default_validity = erlang:error({required, default_validity}),
    max_validity = erlang:error({required, max_validity})
}).

-record(authresponse, {
    result = erlang:error({required, result}),
    customer,
    error
}).

-record(subscribeevent, {
    subscribe_id = erlang:error({required, subscribe_id}),
    queue_name = erlang:error({required, queue_name}),
    customer_id = erlang:error({required, customer_id}),
    user_id = erlang:error({required, user_id})
}).

-record(unsubscribeevent, {
    subscribe_id = erlang:error({required, subscribe_id}),
    reason = erlang:error({required, reason})
}).

