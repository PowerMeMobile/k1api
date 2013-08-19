-module(k1api_utils).

-export([
	addr_to_dto/1
]).

-include("logging.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").

%% ===================================================================
%% Addr to dto
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
