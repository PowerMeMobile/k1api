-module(k1api_batch_srv).

-define(gv(K, P), proplists:get_value(K, P)).

-behaviour(gen_server).

-export([
	start_link/0,
	test/1
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-export([test/1]).

-include_lib("k1api_proto/include/FunnelAsn.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include("gen_server_spec.hrl").

-record(state, {
	chan :: pid()
}).

test(Number) ->
	gen_server:cast(?MODULE, {send_test_mes, Number}).

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Chan = k1api_amqp_pool:open_channel(),
%	link(Chan),
	amqp_channel:register_confirm_handler(Chan, self()),
    ok = k1api_amqp_funs:confirm_select(Chan),
	% declare batch queue
%	{ok, BatchQ} = application:get_env(batch_queue),
	BatchQ = <<"test_queue">>,
	ok = k1api_amqp_funs:queue_declare(Chan, BatchQ),
	{ok, #state{chan = Chan}}.

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast({send_test_mes, Number}, State = #state{chan = Chan}) ->
	Publish = fun() ->
		ok = k1api_amqp_funs:basic_publish(Chan, <<"test_queue">>, <<"payload">>, #'P_basic'{}),
		NextPublish = amqp_channel:next_publish_seqno(Chan),
		?log_debug("NextPublish: ~p", [NextPublish])
	end,
	[Publish() || _MesNumber <- lists:seq(1, Number)],
	{noreply, State};

handle_cast(Req, State) ->
    {stop, {unexpected_cast, Req}, State}.

handle_info(Confirm, St) when is_record(Confirm, 'basic.ack');
                              is_record(Confirm, 'basic.nack') ->
    {noreply, handle_confirm(Confirm, St)};

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

handle_confirm(#'basic.ack'{delivery_tag = DTag, multiple = Multiple}, St) ->
	?log_debug("[BA] dtag: ~p,  multi: ~p", [DTag, Multiple]),
    %% case Multiple of
    %%     false -> do_handle_ack(DTag, St);
    %%     true -> [ do_handle_ack(D, St) || D <- dtags_upto(DTag, St) ]
    %% end,
    St;

handle_confirm(#'basic.nack'{delivery_tag = DTag, multiple = Multiple}, St) ->
	?log_debug("[BN] dtag: ~p,  multi: ~p", [DTag, Multiple]),
    %% case Multiple of
    %%     false -> do_handle_nack(DTag, St);
    %%     true -> lists:foldl(fun(D, S) -> do_handle_nack(D, S) end,
    %%                          St, dtags_upto(DTag, St))
    %% end.
	St.
