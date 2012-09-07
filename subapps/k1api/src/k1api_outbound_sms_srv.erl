-module(k1api_outbound_sms_srv).

%% -define(gv(K, P), proplists:get_value(K, P)).

-behaviour(gen_server).

-export([
	start_link/0,
	send/1
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

%% -export([test/1]).

%-include_lib("k1api_proto/include/FunnelAsn.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("k1api_proto/include/JustAsn.hrl").
-include_lib("k1api_proto/include/FunnelAsn.hrl").
-include("gen_server_spec.hrl").
-include("logging.hrl").

-define(gv(K, P), proplists:get_value(K, P)).

-record(state, {
	chan :: pid()
}).

-record(task, {
	sms_req :: #outbound_sms{},
	customer :: #'Customer'{},
	creds :: #credentials{}
}).

%% -record(send_message, {
%% 	payload :: binary(),
%% 	kelly 	:: binary(),
%% 	just 	:: binary()
%% }).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(#outbound_sms{}, #'Customer'{}, #credentials{}) -> ok.
send(OutboundSms = #outbound_sms{
						%% address = Address,
						%% sender_address = SenderAddr,
						%% message = Message,
						%% sender_name = SenderName, % opt
						%% notify_url = NotifyURL, % opt
						%% client_correlator = Correlator, %opt
						%% callback_data = Callback % opt
										}, Customer = #'Customer'{}, Creds = #credentials{}) ->
	check_client_correlator(#task{sms_req = OutboundSms, customer = Customer, creds = Creds}).

check_client_correlator(OutboundSms = #outbound_sms{
											client_correlator = ClientCorrelator}) ->
	case k1api_cache:fetch(ClientCorrelator) of
		{ok, RequestId} -> {already_sent, RequestId};
		{error, not_found} -> check_encoding(OutboundSms)
	end.

check_encoding(OutboundSms = #outbound_sms{message = Message}) ->
	case gsm0338_pure:from_utf8(Message) of
		{gsm, Binary} ->
			?log_debug("Encoding: gsm", []),
			check_size(OutboundSms, {gsm, Binary});
		{utf8, Binary} ->
			?log_debug("Encoding: utf8", []),
			ok;
			%% check_size(OutboundSms, #message{encoding = utf8, payload = Binary});
		{error, GSM, RestData} ->
			?log_error("Error detecting encoding", []),
			{error, wrong_message_payload};
		{incomplete, GSM, Incomplete} -> ok
	end.

check_size(OutboundSms, {Encoding, Bin}) ->
	case erlang:size(Bin) > 140 of
		false -> send;
		true -> {error, out_of_max_message_size}
	end.

%% ===================================================================
%% GenServer Callback Functions Definitions
%% ===================================================================

init([]) ->
	% open amqp channel
	Chan = k1api_amqp_pool:open_channel(),
	link(Chan),
	% delivery confirm
	amqp_channel:register_confirm_handler(Chan, self()),
    ok = k1api_amqp_funs:confirm_select(Chan),
	% declare batch queue
	BatchQueue = k1api_conf:get_env(queue_backend_batches),
	ok = k1api_amqp_funs:queue_declare(Chan, BatchQueue),
	{ok, #state{chan = Chan}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

%% handle_cast(#send_message{payload = Payload, kelly = KellyQ, just = JustQ}, St = #state{chan = Chan}) ->
%% 	ok = k1api_amqp_funs:basic_publish(Chan, JustQ, Payload, #'P_basic'{}),
%% 	?log_debug("NextPublish after send just: ~p", [amqp_channel:next_publish_seqno(Chan)]),
%% 	ok = k1api_amqp_funs:basic_publish(Chan, KellyQ, Payload, #'P_basic'{}),
%% 	%% NextPublish = amqp_channel:next_publish_seqno(Chan),
%% 	?log_debug("NextPublish after send kelly: ~p", [amqp_channel:next_publish_seqno(Chan)]),
%% 	{noreply, St};

handle_cast(Req, State) ->
    {stop, {unexpected_cast, Req}, State}.

handle_info(Confirm, State = #state{}) when is_record(Confirm, 'basic.ack');
                              				is_record(Confirm, 'basic.nack') ->
    {noreply, handle_confirm(Confirm, State)};

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

handle_confirm(#'basic.ack'{delivery_tag = DTag, multiple = Multiple}, St) ->
	?log_debug("[BA] dtag: ~p,  multi: ~p", [DTag, Multiple]),
     St;

handle_confirm(#'basic.nack'{delivery_tag = DTag, multiple = Multiple}, St) ->
	?log_debug("[BN] dtag: ~p,  multi: ~p", [DTag, Multiple]),
 	St.



publish_batch(Task = #send_sms_req{}, Chan) ->
			BatchId = ,
            Dests = lists:usort(re:split(DestsBin, "/", [trim])),
            TotalDests = 1,
            GtwId = case TotalDests < k1api_conf:get_env(bulk_threshold) of
                        true  -> "gateway_id";
                        false -> "bulk_gateway_id"
                    end,
            Prio = 1,
            Headers = [{request_id, BatchId},
                       {customer_id, ?gv("customer_uuid", Common)},
                       {connection_id, ?gv("connection_id", Common)},
                       {user_id, ?gv("user_id", Common)},
                       {network_id, ?gv("network_id", Common)},
                       {provider_id, ?gv("provider_id", Common)},
                       {gateway_id, GtwId},
                       {priority, Prio},
                       {parts_total, Total},
                       {published_at, fun_time:utc_str()}],
            Basic = #'P_basic'{
                content_type = <<"SmsRequest">>,
                delivery_mode = 2,
                priority = Prio,
                message_id = BatchId,
                headers = lists:map(fun(KV) -> encode_header(KV) end, Headers)
            },
            Payload = encode_batch(Common, Dests, BatchId, GtwId),
            BatchQueue = k1api_conf:get_env(queue_backend_batches),
            GtwPattern = k1api_conf:get_env(queue_gateway_pattern),
            GtwQueue = re:replace(GtwPattern, "%id%", GtwId, [{return, binary}]),
            ok = k1api_amqp_funs:basic_publish(Chan, BatchQueue, Payload, Basic),
            ok = k1api_amqp_funs:basic_publish(Chan, GtwQueue, Payload, Basic).

encode_batch(Common, Dests, BatchId, GtwId) ->
    TS = ?gv("sar_total_segments", Common),
    SS = ?gv("sar_segment_seqnum", Common),
    Type = case TS =:= -1 andalso SS =:= -1 of true -> regular; false -> part end,
    {MsgIds, DestAsns} = lists:foldl(
        fun(Bin, {Ids, Asns}) ->
            [MsgId, RefNum, Addr, Ton, Npi] =
                re:split(Bin, ";", [trim]),
            FA = #'FullAddr'{addr = Addr,
                             ton = list_to_integer(binary_to_list(Ton)),
                             npi = list_to_integer(binary_to_list(Npi))},
            case Type of
                regular ->
                    {[MsgId|Ids], [FA|Asns]};
                part ->
                    {[MsgId|Ids],
                     [#'FullAddrAndRefNum'{
                          fullAddr = FA,
                          refNum = list_to_integer(binary_to_list(RefNum))
                      }|Asns]}
            end
        end,
        {[], []},
        Dests
    ),
    ParamsBase = [{registered_delivery, false,
                  {service_type, []},
                  {no_retry, true},
                  {validity_period, []},
                  {priority_flag, 0},
                  {esm_class, 3},
                  {protocol_id, 0}],
    ParamsSar = case Type of
                    regular ->
                        ParamsBase;
                    part ->
                        [{sar_total_segments, TS}, {sar_segment_seqnum, SS}|ParamsBase]
                end,
    DataCoding = ?gv("data_coding", Common),
    Params = case DataCoding of
                 240 -> [{data_coding, 240}|ParamsSar];
                 _   -> ParamsSar
             end,
    ReqAsn = #'SmsRequest'{
        id = BatchId,
        gatewayId = GtwId,
        customerId = ?gv("customer_uuid", Common),
        type = Type,
        message = ?gv("short_message", Common),
        encoding =
            case DataCoding of
                DC when DC =:= 0; DC =:= 1; DC =:= 3; DC =:= 240 ->
                    {text, default};
                8 ->
                    {text, ucs2};
                DC ->
                    {other, DC}
            end,
        params = [ asn_param(P) || P <- Params ],
        sourceAddr = #'FullAddr'{
            addr = ?gv("source_addr", Common),
            ton = ?gv("source_addr_ton", Common),
            npi = ?gv("source_addr_npi", Common)
        },
        destAddrs = {Type, DestAsns},
        messageIds = MsgIds
    },
    {ok, EncodedReq} = 'JustAsn':encode('SmsRequest', ReqAsn),
    list_to_binary(EncodedReq).


encode_header({K, V}) ->
    {Type, Val} =
        case V of
            true  -> {longstr, <<"true">>};
            false -> {longstr, <<"false">>};
            Int when is_integer(Int) -> {signedint, Int};
            Str   -> {longstr, Str}
        end,
    {erlang:atom_to_binary(K, latin1), Type, Val}.


asn_param({K, V}) ->
    #'Param'{name = atom_to_list(K),
             value = case V of
                         I when is_integer(I) ->
                             {integer, I};
                         B when is_boolean(B) ->
                             {boolean, B};
                         S ->
                             {string, S}
                     end}.
