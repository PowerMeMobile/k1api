-module(oneapi_srv_utils).

-export([
    translate_status_name/1,
    reformat_addr/1,
    reformat_addrs/1
]).

-include_lib("alley_dto/include/adto.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec translate_status_name(binary()) -> binary().
translate_status_name(<<"submitted">>) ->
    <<"MessageWaiting">>;
translate_status_name(<<"success_waiting_delivery">>) ->
    <<"MessageWaiting">>;
translate_status_name(<<"success_no_delivery">>) ->
    <<"DeliveredToNetwork">>;
translate_status_name(<<"failure">>) ->
    <<"DeliveryImpossible">>;
translate_status_name(<<"enroute">>) ->
    <<"DeliveredToNetwork">>;
translate_status_name(<<"delivered">>) ->
    <<"DeliveredToTerminal">>;
translate_status_name(<<"expired">>) ->
    <<"DeliveryImpossible">>;
translate_status_name(<<"deleted">>) ->
    <<"DeliveryImpossible">>;
translate_status_name(<<"undeliverable">>) ->
    <<"DeliveryImpossible">>;
translate_status_name(<<"accepted">>) ->
    <<"DeliveredToNetwork">>;
translate_status_name(<<"unknown">>) ->
    <<"DeliveryUncertain">>;
translate_status_name(<<"rejected">>) ->
    <<"DeliveryImpossible">>;
translate_status_name(<<"unrecognized">>) ->
    <<"DeliveryImpossible">>.

-spec reformat_addr(binary()) -> #addr{}.
reformat_addr(<<"tel:+", Addr/binary>>) ->
    reformat_addr(Addr);
reformat_addr(<<"tel:", Addr/binary>>) ->
    reformat_addr(Addr);
reformat_addr(Addr) ->
    alley_services_utils:addr_to_dto(Addr).

-spec reformat_addrs([binary()]) -> [#addr{}].
reformat_addrs(Addrs) ->
    [reformat_addr(Addr) || Addr <- Addrs].
