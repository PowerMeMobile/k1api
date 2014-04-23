-module(eoa_error_handler).

-behaviour(cowboy_http_handler).

-export([
	init/3,
	handle/2,
	terminate/3
]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Text = <<"Not found: mistake in the host or path of the service URI">>,
	{ok, Req2} = cowboy_req:reply(404, [], Text, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
