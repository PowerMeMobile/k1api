-module(oabc_batch_consum_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
    chan :: pid(),
    queue :: binary(),
    tag :: binary(),
    callback_fun %:: fun(binary(), [{atom(), term()}) -> ok
    }).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    start_batch_receiver/1
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_batch_receiver(Fun) when is_function(Fun)->
    gen_server:cast(?MODULE, {subscribe, Fun}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    Chan = oabc_amqp_pool:open_channel(),
    link(Chan),
    {ok, QName} = application:get_env(queue_server_subsciptions),
    ok = oabc_amqp:queue_declare(Chan, QName, true, false, false),
    QoS = 
    case application:get_env(queue_server_subsciptions_qos) of
        {ok, Value} when is_integer(Value) -> Value; 
        Error -> 1000
    end,
    ok = oabc_amqp:basic_qos(Chan, QoS),
    {ok, #state{chan = Chan, queue = QName}}.

handle_call({unsubscribe}, From, State = #state{chan = Chan, tag = ConsumerTag}) ->
    {ok, ConsumerTag} = oabc_amqp:basic_cancel(Chan, ConsumerTag),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({subscribe, Fun}, State = #state{chan = Chan, queue = QName}) ->
    {ok, ConsumerTag} = oabc_amqp:basic_consume(Chan, QName, false),
    {noreply, State#state{chan = Chan, tag = ConsumerTag, callback_fun = Fun}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({#'basic.deliver'{delivery_tag = DeliveryTag},#amqp_msg{payload = Content}},
                    State = #state{callback_fun = Fun, chan = Chan}) ->
    ?log_debug("got message [~p bytes]. trying to process", [size(Content)]),
    case Fun(Content, []) of
        ok ->
            oabc_amqp:basic_ack(Chan, DeliveryTag);
        Error ->
            ?log_error("Payload process error: ~p", [Error])
    end,
    {noreply, State#state{}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
