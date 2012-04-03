-module(oabc_req_worker).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include("oabc.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
	id :: term(),
	chan :: pid(),
	fw_q :: binary(),
	bw_q :: binary(),
	correlation_id :: binary()
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

init(Spec = #peer_spec{id = Id, chan = Chan, fw_q = FQ, bw_q = BQ}) ->
	{ok, #state{id = Id, chan = Chan, fw_q = FQ, bw_q = BQ}}.

handle_call({send, Payload}, _From, State = #state{id = Id, fw_q = FQ, bw_q = BQ, chan = Chan}) ->
	MsgId = oabc_uuid:newid(),
	?log_debug("MsgId: ~p", [MsgId]),
	?log_debug("BQ ~p", [BQ]),
	Props = #'P_basic'{
					message_id = MsgId,
					correlation_id = MsgId,
					reply_to = BQ
					},
	ok = oabc_amqp:basic_publish(Chan, FQ, Payload, Props),
	?log_debug("basic_publish ok", []),
	Response = oabc_consumer_srv:get_response(Id, MsgId),
	{stop, normal, Response, State#state{}};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------