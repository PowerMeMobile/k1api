-module(oneapi_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

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
        {oneapi_srv_db, {oneapi_srv_db, start_link, []},
            permanent, 5000, worker, [oneapi_srv_db]},
        {oneapi_srv_incoming_sms, {oneapi_srv_incoming_sms, start_link, []},
            permanent, 5000, worker, [oneapi_srv_incoming_sms]}
    ]}}.
