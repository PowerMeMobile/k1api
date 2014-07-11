-module(oneapi_srv_db).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    next_id/1,
    next_id/2,
    check_correlator/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").

-type customer_id()   :: binary().
-type user_id()       :: binary().
-type correlator_id() :: binary().
-type request_id()    :: binary().

%% Customer Last MessageID Storage
-record(customer_next_message_id, {
    customer_id :: customer_id(),
    next_id     :: integer()
}).

%% Customer Request Correlator Storage
-type correlator_key()   :: {customer_id(), user_id(), correlator_id()}.
-type correlator_value() :: request_id().

-record(correlator, {
    key        :: correlator_key(),
    value      :: correlator_value(),
    created_at :: integer()
}).

-record(st, {}).

-define(PurgeRate, 10000). %% milliseconds
-define(CorrelatorLifetime, 60). %% seconds

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec next_id(customer_id()) -> {ok, [integer()]}.
next_id(CustomerID) ->
    next_id(CustomerID, 1).

-spec next_id(customer_id(), NumberOfIDs :: integer()) -> {ok, [integer()]}.
next_id(CustomerID, NumberOfIDs) ->
    {atomic, IDs} = mnesia:transaction(fun() ->
        case mnesia:read(customer_next_message_id, CustomerID, write) of
            [] ->
                update_counter(1, NumberOfIDs, CustomerID);
            [#customer_next_message_id{next_id = NextID}] ->
                update_counter(NextID, NumberOfIDs, CustomerID)
        end
    end),
    {ok, IDs}.

-spec check_correlator(
    customer_id(), user_id(), undefined | correlator_id(), request_id()
) -> ok | {correlator_exist, request_id()}.
check_correlator(_, _, undefined, _) -> ok;
check_correlator(CustomerID, UserID, CorrelatorID, NewRequestID) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        Key = {CustomerID, UserID, CorrelatorID},
        case mnesia:read(correlator, Key, write) of
            [] ->
                CreatedAt = ac_datetime:utc_unixepoch(),
                mnesia:write(#correlator{
                    key = Key,
                    value = NewRequestID,
                    created_at = CreatedAt
                }),
                ok;
            [#correlator{value = RequestID}] ->
                {correlator_exist, RequestID}
        end
    end),
    Result.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ok = init_mnesia(),
    {ok, #st{}, ?PurgeRate}.

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(timeout, State) ->
    TimeThreshold  = ac_datetime:utc_unixepoch() - ?CorrelatorLifetime,
    MatchHead = #correlator{key = '$1', value = '_', created_at = '$2'},
    Guard = {'<', '$2', TimeThreshold},
    Result = '$1',
    Delete = fun(ID) -> ok = mnesia:dirty_delete(correlator, ID) end,
    IDs = mnesia:dirty_select(correlator, [{MatchHead, [Guard], [Result]}]),
    lists:foreach(Delete, IDs),
    {noreply, State, ?PurgeRate};

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

update_counter(NextID, NumberOfIDs, CustomerID) ->
    Max = 999999999,
    {From, To, NewNextID} =
    case (NextID + NumberOfIDs - 1) > Max of
        true -> {1, NumberOfIDs, NumberOfIDs + 1};
        false -> {NextID, NextID + NumberOfIDs - 1, NextID + NumberOfIDs}
    end,
    IDs = lists:seq(From, To),
    mnesia:write(#customer_next_message_id{
        customer_id = CustomerID,
        next_id = NewNextID
    }),
    IDs.

init_mnesia() ->
    Nodes = [node()],
    mnesia:set_debug_level(verbose),
    mnesia:stop(),
    ?log_debug("Creating mnesia schema on: ~p...", [Nodes]),
    ok = case mnesia:create_schema(Nodes) of
        ok ->
            ?log_debug("Mnesia schema was created", []),
            ok;
        {error, {MnesiaNode, {already_exists, MnesiaNode}}} ->
            MnesiaNodes = mnesia:system_info(db_nodes),
            case lists:member(MnesiaNode, MnesiaNodes) of
                true ->
                    ?log_debug("Mnesia schema already exists on: ~p", [MnesiaNode]),
                    ok;
                false ->
                    ?log_error("Mnesia schema already exists on: ~p, but it's not in existing list: ~p",
                        [MnesiaNode, MnesiaNodes]),
                    ?log_error("Did you rename the node?", []),
                    erlang:error(schema_already_exists_created_on_different_node)
            end
    end,
    ok = mnesia:start(),

    ok = ensure_table(customer_next_message_id,
        record_info(fields, customer_next_message_id)),

    ok = ensure_table(correlator,
        record_info(fields, correlator)).

ensure_table(TableName, RecordInfo) ->
    ok =
        case mnesia:create_table(TableName, [
                {disc_copies, [node()]},
                {attributes, RecordInfo}]) of
            {atomic, ok} ->
                ok;
            {aborted, {already_exists, TableName}} ->
                ok
        end,
    ok = mnesia:wait_for_tables([TableName], infinity).
