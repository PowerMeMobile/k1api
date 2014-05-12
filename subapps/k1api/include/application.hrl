-ifndef(k1api_application_hrl).
-define(k1api_application_hrl, defined).

-define(APP, k1api).

-record(pworker, {
    id        :: term(),
    timestamp :: integer(),
    from      :: {pid(), term()}
}).

-record(presponse, {
    id        :: term(),
    timestamp :: integer(),
    response  :: term()
}).

-endif.
