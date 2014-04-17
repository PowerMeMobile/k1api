-module(soap_srv_auth_cache).

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

%% API exports
-export([
    start_link/0,
    store/4,
    fetch/3
    %% delete/1
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include("logging.hrl").

-define(SYNC_INTERVAL, (1000 * 60 * 5)).

-record(sync,{
    ref :: reference()
}).

-record(st, {
    ref :: reference()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec store(binary(), binary(), binary(), tuple()) -> ok.
store(CustomerID, UserID, Password, Customer) ->
    Key = {
        CustomerID,
        UserID,
        Password
    },
    gen_server:call(?MODULE, {store, Key, Customer}).

-spec fetch(binary(), binary(), binary()) ->
    {ok, record()} | not_found.
fetch(CustomerID, UserID, Password) ->
    Key = {
        CustomerID,
        UserID,
        Password
    },
    gen_server:call(?MODULE, {fetch, Key}).

%% -spec delete(any()) -> ok.
%% delete(Key) ->
%%     gen_server:call(?MODULE, {delete, Key}, infinity).

%% ===================================================================
%% GenServer Callback
%% ===================================================================

init([]) ->
    process_flag(trap_exit, true),
    DetsOpts = [{ram_file, true}, {file, "data/auth_cache.dets"}],
    {ok, ?MODULE} = dets:open_file(?MODULE, DetsOpts),
    Ref = erlang:make_ref(),
    erlang:send_after(?SYNC_INTERVAL, self(), #sync{ref = Ref}),
    ?log_info("auth_cache: started", []),
    {ok, #st{ref = Ref}}.

handle_call({store, Key, Value}, _From, St) ->
    ok = dets:insert(?MODULE, {Key, Value}),
    {reply, ok, St};

handle_call({fetch, Key}, _From, St) ->
    case dets:lookup(?MODULE, Key) of
        [] ->
            {reply, not_found, St};
        [{Key, Value}] ->
            {reply, {ok, Value}, St}
    end;

handle_call({delete, Key}, _From, St) ->
    dets:delete(?MODULE, Key),
    {reply, ok, St};

handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.

handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.

handle_info(#sync{ref = Ref}, St = #st{ref = Ref}) ->
    ok = dets:sync(?MODULE),
    %?log_info("auth_cache: sync", []),
    erlang:send_after(?SYNC_INTERVAL, self(), #sync{ref = Ref}),
    {noreply, St};
handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

terminate(Reason, _St) ->
    dets:close(?MODULE),
    ?log_info("auth_cache: terminated (~p)", [Reason]).
