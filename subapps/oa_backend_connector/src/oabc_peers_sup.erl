-module(oabc_peers_sup).

-behaviour(supervisor).

-include("oabc.hrl").

%% API
-export([
	start_link/0,
	start_child/1
	]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Spec = #peer_spec{}) ->
	supervisor:start_child(?MODULE, [Spec]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, { {simple_one_for_one, 5, 10}, [
		{peer_sup, {oabc_peer_sup, start_link, []}, permanent, infinity, supervisor, [oabc_peer_sup]}
	]} }.