-module(oabc_req_sup).

-behaviour(supervisor).

-include("oabc.hrl").

%% API
-export([
	start_link/1
	]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Spec) ->
	supervisor:start_link(?MODULE, Spec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Spec = #peer_spec{}) ->
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	{ok, { {simple_one_for_one, 5, 10}, [
		{ req_worker, {oabc_req_worker, start_link, [Spec#peer_spec{chan = Chan}]}, transient, brutal_kill, worker, [oabc_req_worker]}
	]} }.