-module(oneapi_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {k1api_db, {k1api_db, start_link, []},
            permanent, 5000, worker, [k1api_db]},
        {k1api_incoming_srv, {k1api_incoming_srv, start_link, []},
            permanent, 5000, worker, [k1api_incoming_srv]}
    ]}}.
