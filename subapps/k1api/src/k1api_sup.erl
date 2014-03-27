-module(k1api_sup).

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
        {k1api_auth_cache, {k1api_auth_cache, start_link, []},
            permanent, 5000, worker, [k1api_auth_cache]},
        {k1api_db, {k1api_db, start_link, []},
            permanent, 5000, worker, [k1api_db]},
        {k1api_auth_srv, {k1api_auth_srv, start_link, []},
            permanent, 5000, worker, [k1api_auth_srv]},
        {k1api_delivery_status_srv, {k1api_delivery_status_srv, start_link, []},
            permanent, 5000, worker, [k1api_delivery_status_srv]},
        {k1api_retrieve_sms_srv, {k1api_retrieve_sms_srv, start_link, []},
            permanent, 5000, worker, [k1api_retrieve_sms_srv]},
        {k1api_outbound_sms_srv, {k1api_outbound_sms_srv, start_link, []},
            permanent, 5000, worker, [k1api_outbound_sms_srv]},
        {k1api_subscription_srv, {k1api_subscription_srv, start_link, []},
            permanent, 5000, worker, [k1api_subscription_srv]},
        {k1api_incoming_srv, {k1api_incoming_srv, start_link, []},
            permanent, 5000, worker, [k1api_incoming_srv]},
        {k1api_billy_session, {k1api_billy_session, start_link, []},
            transient, 5000, worker, [k1api_billy_session]}
    ]}}.
