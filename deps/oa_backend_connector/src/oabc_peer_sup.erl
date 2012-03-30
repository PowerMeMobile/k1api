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
	supervisor:start_link(?MODULE, {Type, Spec}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args = {'2way', Spec = #peer_spec{}}) ->
	?log_debug("~p", [Args]),
	{ok, { {one_for_one, 5, 10}, [
		{consumer_srv, {oabc_consumer_srv, start_link, [Spec]}, permanent, 5000, worker, [oabc_consumer_srv]},
		{req_sup, {oabc_req_sup, start_link, [Spec]}, permanent, infinity, supervisor, [oabc_req_sup]}
	]} };
init({'fw', Spec = #peer_spec{}}) ->
	{ok, { {one_for_one, 5, 10}, [
		{req_sup, {oabc_req_sup, start_link, [Spec]}, permanent, infinity, supervisor, [oabc_req_sup]}
	]} };
init({'bw', Spec = #peer_spec{}}) ->
	{ok, { {one_for_one, 5, 10}, [
		{consumer_srv, {oabc_consumer_srv, start_link, [Spec]}, permanent, 5000, worker, [oabc_consumer_srv]}
	]} }.