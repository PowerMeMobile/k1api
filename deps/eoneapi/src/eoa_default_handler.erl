-module(eoa_default_handler).
-behaviour(cowboy_http_handler).
-compile([{parse_transform, lager_transform}]).
-include("logging.hrl").
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	?log_debug("Bad req: ~p", [Req]),
	{ok, Req2} = cowboy_http_req:reply(404, [], <<"Not found: mistake in the host or path of the service URI">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.