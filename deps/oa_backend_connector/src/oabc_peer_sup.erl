-module(oabc_peer_sup).

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

-include("oabc.hrl").
-include("logging.hrl").

%% API
-export([
	start_link/1
	]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Spec = #peer_spec{type = Type}) ->
	supervisor:start_link(?MODULE, Spec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Spec = #peer_spec{type = '2way'}) ->
	{ok, { {one_for_one, 5, 10}, [
		{consumer_srv, {oabc_consumer_srv, start_link, [Spec]}, permanent, 5000, worker, [oabc_consumer_srv]},
		{req_sup, {oabc_req_sup, start_link, [Spec]}, permanent, infinity, supervisor, [oabc_req_sup]}
	]} };
init(Spec = #peer_spec{type = fw}) ->
	{ok, { {one_for_one, 5, 10}, [
		{req_sup, {oabc_req_sup, start_link, [Spec]}, permanent, infinity, supervisor, [oabc_req_sup]}
	]} };
init(Spec = #peer_spec{type = bw}) ->
	{ok, { {one_for_one, 5, 10}, [
		{consumer_srv, {oabc_bw_srv, start_link, [Spec]}, permanent, 5000, worker, [oabc_bw_srv]}
	]} }.