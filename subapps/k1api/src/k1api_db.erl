-module(k1api_db).

%% API
-export([
	init/0,
	next_id/1, next_id/2
]).

-include("logging.hrl").

-record(customer_next_message_id, {
	customer_id :: binary(), %% <<23,23...>> uuid
	next_id :: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

init() ->
	Nodes = [node()],
	%% mnesia:set_debug_level(verbose),
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

	ok = ensure_table(
		customer_next_message_id,
		record_info(fields, customer_next_message_id)).

next_id(CustomerID) ->
	next_id(CustomerID, 1).
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

update_counter(NextID, NumberOfIDs, CustomerID) ->
	Max = 999999999,
	{From, To, NewNextID} =
	case (NextID + NumberOfIDs - 1) > Max of
		true -> {1, NumberOfIDs, NumberOfIDs + 1};
		false -> {NextID, NextID + NumberOfIDs - 1, NextID + NumberOfIDs}
	end,
	IDs = lists:seq(From, To),
	mnesia:write(#customer_next_message_id{customer_id = CustomerID, next_id = NewNextID}),
	IDs.



%% ===================================================================
%% Local Functions
%% ===================================================================

get_nodes() -> [node()].

ensure_table(TableName, RecordInfo) ->
	ok = case mnesia:create_table(TableName, [
					{disc_copies, get_nodes()},
					{attributes, RecordInfo}]) of
			{atomic, ok} ->
				ok;
			{aborted, {already_exists, TableName}} ->
				ok
		 end,
    ok = mnesia:wait_for_tables([TableName], infinity).

ensure_table(TableName, RecordName, RecordInfo) ->
	ok = case mnesia:create_table(TableName, [
					{disc_copies, get_nodes()},
					{record_name, RecordName},
					{attributes, RecordInfo}]) of
			{atomic, ok} ->
				ok;
			{aborted, {already_exists, TableName}} ->
				ok
		 end,
    ok = mnesia:wait_for_tables([TableName], infinity).
