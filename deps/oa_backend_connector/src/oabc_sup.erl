
-module(oabc_sup).

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
		?WORKER(oabc_uuid, permanent, 5000),
		?WORKER(oabc_amqp_pool, permanent, 5000),
		{peers_sup, {oabc_peers_sup, start_link, []}, permanent, infinity, supervisor, [oabc_peers_sup]}
	]} }.



		% ?WORKER(oabc_ctrl_consum_srv, permanent, 5000),
		% ?WORKER(oabc_producer_srv, permanent, 5000),
		% ?SUPERVISOR(oabc_batch_consum_srv_sup, permanent)