
-module(oa_backend_connector_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(Mod, Restart, Shutdown),
    {Mod, {Mod, start_link, []}, Restart, Shutdown, worker, [Mod]}).
-define(SUPERVISOR(Mod, Restart),
    {Mod, {Mod, start_link, []}, Restart, infinity, supervisor, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
    	?WORKER(oabc_amqp_pool, permanent, 5000),
    	?SUPERVISOR(oabc_worker_sup, permanent)
    ]} }.

