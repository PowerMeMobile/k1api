%% add expiration period
%% add async

-module(oabc_consumer_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include("oabc.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(pworker, {
	id,
	timestamp,
	from
	}).
-record(presponse, {
	id,
	timestamp,
	payload
	}).
-record(state, {
	chan :: pid(),
	queue :: binary(),
	tag :: binary(),
	pending_workers = [] :: [#pworker{}],
	pending_responses = [] :: [#presponse{}]
	}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/1,
	get_response/2
	]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Spec) ->
    gen_server:start_link(?MODULE, Spec, []).

get_response(Id, CorrelationId) ->
	case gproc:lookup_local_name({oabc_consumer_srv, Id}) of
		Pid when is_pid(Pid) ->
			?log_debug("oabc_consumer_srv: ~p", [Pid]),
			gen_server:call(Pid, {get_response, CorrelationId}, infinity);
		_ -> {error, no_proc}
	end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Spec = #peer_spec{id = Id, bw_q = QName}) ->
	gproc:add_local_name({?MODULE, Id}),
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, QName, true, false, false),
		{ok, ConsumerTag} = oabc_amqp:basic_consume(Chan, QName, false),
	{ok, #state{tag = ConsumerTag, queue = QName, chan = Chan}}.

handle_call({get_response, CorrelationId}, From, 
					State = #state{
								pending_workers = WList,
								pending_responses = RList}) ->
	case lists:keytake(CorrelationId, #presponse.id, RList) of
		{value, #presponse{payload = Payload}, RestRespList} ->
			{reply, Payload, State#state{pending_responses = RestRespList}};
		false ->
			Worker = #pworker{id = CorrelationId, timestamp = now(), from = From},
			{noreply, State#state{pending_workers = [Worker] ++ WList}}
	end;

handle_call(_Request, _From, State) ->
	{stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
	{stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{delivery_tag = DeliveryTag},
				#amqp_msg{props = Props, payload = Payload}},
					State = #state{chan = Chan,
									pending_responses = RList, 
									pending_workers = WList}) ->
	#'P_basic'{message_id = MsgId, correlation_id = CorrelationId, reply_to = ReplyTo} = Props, 
	?log_debug("got message: ~p", [Payload]),
	?log_debug("MsgId: ~p", [MsgId]),
	?log_debug("CorrelationId: ~p", [CorrelationId]),
	?log_debug("ReplyTo: ~p", [ReplyTo]),
		oabc_amqp:basic_ack(Chan, DeliveryTag),
	case lists:keytake(CorrelationId, #pworker.id, WList) of
		{value, #pworker{from = From}, RestWorkerList} ->
			gen_server:reply(From, Payload),
			{noreply, State#state{pending_workers = RestWorkerList}};
		false ->
			Response = #presponse{id = CorrelationId, timestamp = now(), payload = Payload},
			{noreply, State#state{pending_responses = [Response] ++ RList}}
	end;

handle_info(_Info, State) ->
	{stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------