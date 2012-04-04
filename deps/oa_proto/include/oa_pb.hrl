-record(frontendupevent, {
    resetsubscriptions = erlang:error({required, resetsubscriptions}),
    authq = erlang:error({required, authq}),
    registerq = erlang:error({required, registerq}),
    controlq = erlang:error({required, controlq})
}).

-record(authreq, {
    systemid = erlang:error({required, systemid}),
    userid = erlang:error({required, userid}),
    password = erlang:error({required, password}),
    type = erlang:error({required, type}),
    iscached = erlang:error({required, iscached}),
    timestamp = erlang:error({required, timestamp})
}).

