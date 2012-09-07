-module(k1api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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

		{k_mnesia_schema, {k_mnesia_schema, start_link, []}, permanent, 100000, worker, [k_mnesia_schema]},

		?CHILD(k1api_uuid, worker),

		?CHILD(k1api_amqp_pool, worker),

    	?CHILD(k1api_cache, worker),

		?CHILD(k1api_auth_srv, worker),

		?CHILD(k1api_subscription_srv, worker),

		?CHILD(k1api_incoming_srv, worker),

		?CHILD(k1api_batch_srv, worker)

    	]} }.

