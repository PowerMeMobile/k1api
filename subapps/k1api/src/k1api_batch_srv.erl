-module(k1api_batch_srv).

-define(gv(K, P), proplists:get_value(K, P)).

-behaviour(gen_server).

-export([
	start_link/0
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-include("FunnelAsn.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include("gen_server_spec.hrl").

-record(state, {
	chan :: pid()
}).

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Chan = k1api_amqp_pool:open_channel(),
	link(Chan),

	% declare batch queue
	{ok, BatchQ} = application:get_env(batch_queue),
	ok = k1api_amqp_funs:queue_declare(Chan, BatchQ),
	{ok, #state{chan = Chan}}.

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------------------------------------------------------------------
% Internal Function Definitions
% ------------------------------------------------------------------

%% publish_user_batch(Toke, Chan, BatchId) ->
%%     CommonBin = toke_drv:get(Toke, ?TOKYO_BATCH_COMMON(BatchId)),
%%     DestsBin  = toke_drv:get(Toke, ?TOKYO_BATCH_DESTS(BatchId)),
%%     if
%%         CommonBin =/= not_found andalso DestsBin =/= not_found ->
%%             {ok, {obj, Common}, []} = rfc4627:decode(CommonBin),
%%             Dests = lists:usort(re:split(DestsBin, "/", [trim])),
%%             Total = lists:foldl(fun(DestBin, Acc) ->
%%                                     MsgId = hd(re:split(DestBin, ";", [trim])),
%%                                     length(re:split(MsgId, ":", [trim])) + Acc
%%                                 end, 0, Dests),
%%             GtwId = case Total < funnel_conf:get(bulk_threshold) of
%%                         true  -> string:to_lower(?gv("gateway_id", Common));
%%                         false -> string:to_lower(?gv("bulk_gateway_id", Common))
%%                     end,
%%             Prio = ?gv("priority", Common),
%%             Headers = [{request_id, BatchId},
%%                        {customer_id, ?gv("customer_uuid", Common)},
%%                        {connection_id, ?gv("connection_id", Common)},
%%                        {user_id, ?gv("user_id", Common)},
%%                        {network_id, ?gv("network_id", Common)},
%%                        {provider_id, ?gv("provider_id", Common)},
%%                        {gateway_id, GtwId},
%%                        {priority, Prio},
%%                        {parts_total, Total},
%%                        {published_at, fun_time:utc_str()}],
%%             Basic = #'P_basic'{
%%                 content_type = <<"SmsRequest">>,
%%                 delivery_mode = 2,
%%                 priority = Prio,
%%                 message_id = BatchId,
%%                 headers = lists:map(fun(KV) -> encode_header(KV) end, Headers)
%%             },
%%             Payload = encode_batch(Common, Dests, BatchId, GtwId),
%%             BatchQueue = funnel_app:get_env(queue_backend_batches),
%%             GtwPattern = funnel_app:get_env(queue_gateway_pattern),
%%             GtwQueue = re:replace(GtwPattern, "%id%", GtwId, [{return, binary}]),
%%             ok = fun_amqp:basic_publish(Chan, BatchQueue, Payload, Basic),
%%             ok = fun_amqp:basic_publish(Chan, GtwQueue, Payload, Basic);
%%         true ->
%%             ok
%%     end.

%% encode_batch(Common, Dests, BatchId, GtwId) ->
%%     TS = ?gv("sar_total_segments", Common),
%%     SS = ?gv("sar_segment_seqnum", Common),
%%     Type = case TS =:= -1 andalso SS =:= -1 of true -> regular; false -> part end,
%%     {MsgIds, DestAsns} = lists:foldl(
%%         fun(Bin, {Ids, Asns}) ->
%%             [MsgId, RefNum, Addr, Ton, Npi] =
%%                 re:split(Bin, ";", [trim]),
%%             FA = #'FullAddr'{addr = Addr,
%%                              ton = list_to_integer(binary_to_list(Ton)),
%%                              npi = list_to_integer(binary_to_list(Npi))},
%%             case Type of
%%                 regular ->
%%                     {[MsgId|Ids], [FA|Asns]};
%%                 part ->
%%                     {[MsgId|Ids],
%%                      [#'FullAddrAndRefNum'{
%%                           fullAddr = FA,
%%                           refNum = list_to_integer(binary_to_list(RefNum))
%%                       }|Asns]}
%%             end
%%         end,
%%         {[], []},
%%         Dests
%%     ),
%%     ParamsBase = [{registered_delivery, ?gv("registered_delivery", Common) > 0},
%%                   {service_type, ?gv("service_type", Common)},
%%                   {no_retry, ?gv("no_retry", Common)},
%%                   {validity_period, ?gv("validity_period", Common)},
%%                   {priority_flag, ?gv("priority_flag", Common)},
%%                   {esm_class, ?gv("esm_class", Common)},
%%                   {protocol_id, ?gv("protocol_id", Common)}],
%%     ParamsSar = case Type of
%%                     regular ->
%%                         ParamsBase;
%%                     part ->
%%                         [{sar_total_segments, TS}, {sar_segment_seqnum, SS}|ParamsBase]
%%                 end,
%%     DataCoding = ?gv("data_coding", Common),
%%     Params = case DataCoding of
%%                  240 -> [{data_coding, 240}|ParamsSar];
%%                  _   -> ParamsSar
%%              end,
%%     ReqAsn = #'SmsRequest'{
%%         id = BatchId,
%%         gatewayId = GtwId,
%%         customerId = ?gv("customer_uuid", Common),
%%         type = Type,
%%         message = ?gv("short_message", Common),
%%         encoding =
%%             case DataCoding of
%%                 DC when DC =:= 0; DC =:= 1; DC =:= 3; DC =:= 240 ->
%%                     {text, default};
%%                 8 ->
%%                     {text, ucs2};
%%                 DC ->
%%                     {other, DC}
%%             end,
%%         params = [ asn_param(P) || P <- Params ],
%%         sourceAddr = #'FullAddr'{
%%             addr = ?gv("source_addr", Common),
%%             ton = ?gv("source_addr_ton", Common),
%%             npi = ?gv("source_addr_npi", Common)
%%         },
%%         destAddrs = {Type, DestAsns},
%%         messageIds = MsgIds
%%     },
%%     {ok, EncodedReq} = 'JustAsn':encode('SmsRequest', ReqAsn),
%%     list_to_binary(EncodedReq).
