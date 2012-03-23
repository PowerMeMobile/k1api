-module(oabc_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(Mod, Restart, Shutdown),
    {Mod, {Mod, start_link, []}, Restart, Shutdown, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	{ok, ConnectionMode} = application:get_env(connection_mode),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConnectionMode]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([subscriptions]) ->
    {ok, { {one_for_one, 5, 10}, [
    	?WORKER(oabc_worker, permanent, 5000)
    ]} };

init([nodes]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
    ]} }.

