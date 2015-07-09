-module(oneapi_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

-define(CHILD(I, Restart, Timeout, Type), {I, {I, start_link, []}, Restart, Timeout, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(alley_services_http_in_logger, permanent, 5000, worker),
        ?CHILD(alley_services_http_out_logger, permanent, 5000, worker),
        ?CHILD(oneapi_srv_db, permanent, 5000, worker),
        ?CHILD(oneapi_srv_incoming_sms, permanent, 5000, worker)
    ]}}.
