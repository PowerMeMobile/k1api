-module(k1api_lib).

-include_lib("alley_dto/include/addr.hrl").
-include("application.hrl").

%% API
-export([
	addr_to_dto/1,
	process_response/3,
	process_worker_request/3,
	get_now/0
]).

%% ===================================================================
%% API
%% ===================================================================

-spec addr_to_dto(binary()) -> addr().
addr_to_dto(SenderAddress) ->
	#addr{
		addr = SenderAddress,
		ton = 1,
		npi = 1
	}.

-spec process_response(#presponse{}, [#presponse{}], [#pworker{}]) ->
	{ok, [#presponse{}], [#pworker{}]}.
process_response(PResponse = #presponse{id = ID, response = Response}, RList, WList) ->
		case lists:keytake(ID, #pworker.id, WList) of
		{value, #pworker{from = From}, RestWorkerList} ->
			gen_server:reply(From, {ok, Response}),
			{ok, purge(RList), purge(RestWorkerList)};
		false ->
			{ok, [PResponse] ++ purge(RList), purge(WList)}
	end.

-spec process_worker_request(#pworker{}, [#presponse{}], [#pworker{}]) ->
	{ok, [#presponse{}], [#pworker{}]}.
process_worker_request(Worker = #pworker{id = ItemID, from = From}, RList, WList) ->
	case lists:keytake(ItemID, #presponse.id, RList) of
		{value, #presponse{response = Response}, RestRespList} ->
			gen_server:reply(From, {ok, Response}),
			{ok, purge(RestRespList), purge(WList)};
		false ->
			{ok, purge(RList), [Worker] ++ purge(WList)}
	end.

-spec get_now() -> integer().
get_now() ->
	 calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%% ===================================================================
%% Internals
%% ===================================================================

purge(List) ->
	{ok, ExpirationInterval} = application:get_env(k1api, request_timeout),
	purge(List, [], get_now() - ExpirationInterval).

purge([], Acc, _Now) -> Acc;
purge([#pworker{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([#presponse{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([Item | RestList], Acc, Now) ->
	purge(RestList, [Item | Acc], Now).
