%% @TODO Add timestamp & and purge expired items
%% @TODO Add transactions
%% @TODO Use ets mutex?

-module(k1api_correlator_cache).

-behaviour(gen_server).

%% API exports
-export([
	start_link/0,
	process/4
]).

%% gen_server exports
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-record(state, {}).
%% -record(correlator, {
%% 	id :: {CustomerID :: binary(), UserID :: binary(),
%% }).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec process(binary(), binary(), binary(), binary()) ->	ok | {exist, RequestID :: binary()}.
process(Customer, User, Correlator, RequestID) ->
	gen_server:call(?MODULE, {process, Customer, User, Correlator, RequestID}).

%% ===================================================================
%% gen_server Callback Functions
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table, {keypos,1}]),
    {ok, #state{}}.

handle_call({process, Customer, User, Correlator, NewRequestID}, _From, State) ->
    case ets:lookup(?MODULE, {Customer, User, Correlator}) of
        [] ->
			true = ets:insert(?MODULE, {{Customer, User, Correlator}, NewRequestID}),
            {reply, ok, State};
        [{{Customer, User, Correlator}, RequestID}] ->
            {reply, {exist, RequestID}, State}
    end;

%% handle_call({delete, Key}, _From, State) ->
%%     ets:delete(cache, Key),
%%     {reply, ok, State};

handle_call(Request, _From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
