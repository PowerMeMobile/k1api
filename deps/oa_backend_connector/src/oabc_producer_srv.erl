-module(oabc_producer_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
    chan :: pid(),
    gateway_queue :: binary(),
    batches_queue :: binary(),
    events_queue :: binary(),
    tag :: binary()
    }).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0
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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    Chan = oabc_amqp_pool:open_channel(),
    link(Chan),
    {ok, GtwQName} = application:get_env(queue_gateway_prefix),
    {ok, BatchQName} = application:get_env(queue_backend_batches),
    oabc_amqp:queue_declare(Chan, BatchQName, true, false, false),
    {ok, EventQName} = application:get_env(queue_backend_events),
    oabc_amqp:queue_declare(Chan, EventQName, true, false, false),
    {ok, #state{chan = Chan,
                gateway_queue = GtwQName,
                batches_queue = BatchQName,
                events_queue = EventQName}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------