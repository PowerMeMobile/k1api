-module(oabc_bw_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include("oabc.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
	id :: term(),
	chan :: pid(),
	queue :: binary(),
	tag :: binary(),
	callback :: atom()
	}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	behaviour_info/1,
	start_link/1
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

behaviour_info(callbacks) ->
	[
		{handle_backward, 2}
	].

start_link(Spec) ->
    gen_server:start_link(?MODULE, Spec, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Spec = #peer_spec{id = Id, bw_q = QName, callback = CallBack}) ->
	gproc:add_local_name({?MODULE, Id}),
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, QName, true, false, false),
		{ok, ConsumerTag} = oabc_amqp:basic_consume(Chan, QName, false),
	{ok, #state{id = Id, tag = ConsumerTag, queue = QName, chan = Chan, callback = CallBack}}.


handle_call(_Request, _From, State) ->
	{stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
	{stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{delivery_tag = DeliveryTag},
				#amqp_msg{props = Props, payload = Payload}},
					State = #state{id = Id, chan = Chan, callback = CallBack}) ->
	#'P_basic'{message_id = MsgId, correlation_id = CorrelationId, reply_to = ReplyTo} = Props, 
	?log_debug("got message: ~p", [Payload]),
	?log_debug("MsgId: ~p", [MsgId]),
	?log_debug("CorrelationId: ~p", [CorrelationId]),
	?log_debug("ReplyTo: ~p", [ReplyTo]),
	case CallBack:handle_backward(Id, Payload) of
		ok ->
			oabc_amqp:basic_ack(Chan, DeliveryTag),
			{noreply, State#state{}};
		Error ->
			?log_error("~p", [Error])
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