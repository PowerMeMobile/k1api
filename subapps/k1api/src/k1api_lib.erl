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

-spec addr_to_dto(Addr :: binary()) -> #addr{}.
addr_to_dto(<<"tel:+", Addr/binary>>) ->
	addr_to_dto(Addr);
addr_to_dto(<<"tel:", Addr/binary>>) ->
	addr_to_dto(Addr);
addr_to_dto(AddrBin) when is_binary(AddrBin) ->
	Addr = binary_to_list(AddrBin),
	Integer =
	try	list_to_integer(Addr) of
		_ -> true
	catch
		_:_ -> false
	end,
	Length = length(Addr),
	addr_to_dto(AddrBin, Integer, Length).

addr_to_dto(AddrBin, true, Length) when Length < 7 ->
	#addr{
		addr = AddrBin,
		ton = 6,
		npi = 0
	};
addr_to_dto(AddrBin, true, Length) when Length > 6 ->
	#addr{
		addr = AddrBin,
		ton = 1,
		npi = 1
	};
addr_to_dto(AddrBin, false, _Length) ->
	#addr{
		addr = AddrBin,
		ton = 5,
		npi = 0
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
