-module(oabc_consumer_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include("oabc.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
	chan :: pid(),
	queue :: binary(),
	tag :: binary()
	}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
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

start_link(Spec) ->
    gen_server:start_link(?MODULE, Spec, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Spec = #peer_spec{bw_q = QName}) ->
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, QName, true, false, false),
		{ok, ConsumerTag} = oabc_amqp:basic_consume(Chan, QName, false),
	{ok, #state{tag = ConsumerTag, queue = QName, chan = Chan}}.
	% QoS = 
	% case application:get_env(queue_server_control_qos) of
	% 	{ok, Value} when is_integer(Value) -> Value; 
	% 	Error -> 1000
	% end,
	% ok = oabc_amqp:basic_qos(Chan, QoS),
	% {ok, #state{}}.

% handle_call({unsubscribe}, _From, State = #state{chan = Chan, tag = ConsumerTag}) ->
%     {ok, ConsumerTag} = oabc_amqp:basic_cancel(Chan, ConsumerTag),
%     {reply, ok, State};
handle_call(_Request, _From, State) ->
	{stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
	{stop, unexpected_cast, State}.

% handle_info({#'basic.deliver'{delivery_tag = DeliveryTag},#amqp_msg{payload = Content}},
% 					State = #state{callback_fun = Fun, chan = Chan}) ->
% 	?log_debug("got message [~p bytes]. trying to process", [size(Content)]),
% 	case Fun(Content, []) of
% 		ok ->
% 			oabc_amqp:basic_ack(Chan, DeliveryTag);
% 		Error ->
% 			?log_error("Payload process error: ~p", [Error])
% 	end,
% 	{noreply, State#state{}};
handle_info(_Info, State) ->
	{stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------