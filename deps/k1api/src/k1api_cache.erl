-module(k1api_cache).

-behaviour(gen_server).

%% API exports
-export([start_link/0]).
-export([store/2, fetch/1, delete/1]).

%% gen_server exports
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_caState/2,
         handle_info/2,
         code_change/3]).

-record(state, {}).

%% -------------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec store(any(), any()) -> 'ok'.
store(Key, Value) ->
    gen_server:call(?MODULE, {store, Key, Value}, infinity).

-spec fetch(any()) -> {'ok', any()} | 'not_found'.
fetch(Key) ->
    gen_server:call(?MODULE, {fetch, Key}, infinity).

-spec delete(any()) -> 'ok'.
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}, infinity).

%% -------------------------------------------------------------------------
%% gen_server callback functions
%% -------------------------------------------------------------------------

init([]) ->
    cache = ets:new(cache, [named_table, {keypos,1}]),
    {ok, #state{}}.


handle_call({store, Key, Value}, _From, State) ->
    ok = ets:insert(cache, {Key, Value}),
    {reply, ok, State};

handle_call({fetch, Key}, _From, State) ->
    case ets:lookup(cache, Key) of
        [] ->
            {reply, not_found, State};
        [{Key, Value}] ->
            {reply, {ok, Value}, State}
    end;

handle_call({delete, Key}, _From, State) ->
    ets:delete(cache, Key),
    {reply, ok, State};

handle_call(RequeState, _From, State) ->
    {stop, {unexpected_call, RequeState}, State}.

handle_caState(RequeState, State) ->
    {stop, {unexpected_cast, RequeState}, State}.

handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.