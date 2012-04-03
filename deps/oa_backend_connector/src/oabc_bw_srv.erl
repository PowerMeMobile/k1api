-module(oabc_bw_srv).

-behaviour(gen_wp).

-compile([{parse_transform, lager_transform}]).

-export([
	behaviour_info/1,
	start_link/1
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2,

	handle_fork_cast/3,
	handle_fork_call/4,
	handle_child_forked/3,
	handle_child_terminated/4
	]).


-include("oabc.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
	id :: term(),
	chan :: pid(),
	queue :: binary(),
	tag :: binary(),
	callback :: atom(),
	workers :: dict()
	}).

behaviour_info(callbacks) ->
	[
		{handle_backward, 2}
	].

start_link(Spec) ->
	gen_wp:start_link(?MODULE, Spec).

init(#peer_spec{id = Id, bw_q = QName, callback = CallBack}) ->
	gproc:add_local_name({?MODULE, Id}),
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, QName, true, false, false),
	{ok, ConsumerTag} = oabc_amqp:basic_consume(Chan, QName, false),
	Workers = dict:new(),
	{ok, #state{id = Id, tag = ConsumerTag, queue = QName, chan = Chan, callback = CallBack, workers = Workers}}.

handle_call(Msg = {send, _Payload}, _From, State = #state{}) ->
	?log_debug("Msg: ~p", [Msg]),
	{fork, {Msg, State}, State#state{}};
handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(ForkInfo = {#'basic.deliver'{}, #amqp_msg{}}, State = #state{}) ->
	{fork, {ForkInfo, State}, State};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_fork_cast(_Arg, 	{{#'basic.deliver'{delivery_tag = DeliveryTag},
							#amqp_msg{props = Props, payload = Payload}},
								#state{id = Id, chan = Chan, callback = CallBack}}, _WP) ->
	#'P_basic'{message_id = MsgId, correlation_id = CorrelationId, reply_to = ReplyTo} = Props, 
	?log_debug("got message: ~p", [Payload]),
	?log_debug("MsgId: ~p", [MsgId]),
	?log_debug("CorrelationId: ~p", [CorrelationId]),
	?log_debug("ReplyTo: ~p", [ReplyTo]),
	case CallBack:handle_backward(Id, Payload) of
		ok ->
			oabc_amqp:basic_ack(Chan, DeliveryTag),
			{noreply, normal};
		Error ->
			?log_error("~p", [Error]),
			{noreply, Error}
	end.

handle_fork_call(_Arg, _Msg, _ReplyTo, _WP) ->
	{noreply, normal}.

handle_child_forked({{#'basic.deliver'{delivery_tag = DTag}, _}, _}, Child, ModState = #state{workers = Workers}) ->
	NWorkers = dict:store(Child, DTag, Workers),
	{noreply, ModState#state{workers = NWorkers}}.

handle_child_terminated(normal, _Msg, Child, ModState = #state{workers = Workers}) ->
	NWorkers = dict:erase(Child, Workers),
	{noreply, ModState#state{workers = NWorkers}};
handle_child_terminated(Error, _Task, Child, ModState = #state{chan = Chan, workers = Workers}) ->
	?log_error("bw process error: ~p", [Error]),
	{ok, DTag} = dict:find(Child, Workers),
	Requeue = true,
	oabc_amqp:basic_reject(Chan, DTag, Requeue),
	NWorkers = dict:erase(Child, Workers),
	{noreply, ModState#state{workers = NWorkers}}.