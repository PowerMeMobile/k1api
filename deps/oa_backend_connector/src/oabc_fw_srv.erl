-module(oabc_fw_srv).

-behaviour(gen_wp).

-compile([{parse_transform, lager_transform}]).

-export([
	start_link/1
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2,

	handle_fork_cast/3,
	handle_fork_call/4,
	handle_child_forked/3,
	handle_child_terminated/4
	]).


-include("oabc.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
	id :: term(),
	type :: '2way' | forward | backward,
	chan :: pid(),
	fw_q :: binary(),
	bw_q :: binary(),
	correlation_id :: binary()
	}).

start_link(Spec = #peer_spec{}) ->
	gen_wp:start_link(?MODULE, Spec).

init(#peer_spec{id = Id, fw_q = FQ, bw_q = BQ, type = Type}) ->
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, FQ, true, false, false),
	gproc:add_local_name({?MODULE, Id}),
	{ok, #state{id = Id, chan = Chan, fw_q = FQ, bw_q = BQ, type = Type}}.

handle_call(Msg = {send, _Payload}, _From, State = #state{}) ->
	?log_debug("Msg: ~p", [Msg]),
	{fork, {Msg, State}, State#state{}};
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

handle_fork_cast(_Arg, _Msg, _WP) ->
	{noreply, normal}.

handle_fork_call(_Arg, {{send, Payload}, #state{type = forward,
												fw_q = FQ,
												chan = Chan}}, _ReplyTo, _WP) ->
	MsgId = oabc_uuid:newid(),
	?log_debug("MsgId: ~p", [MsgId]),
	Props = #'P_basic'{
					message_id = MsgId,
					correlation_id = MsgId
					% content_type
					% content_encoding = undefined,
					% delivery_mode = 2,
					% reply_to
					% expiration
					% timestamp
					% app_id
					},
	Result = oabc_amqp:basic_publish(Chan, FQ, Payload, Props),
	?log_debug("basic_publish ok", []),
	{reply, Result, normal};

handle_fork_call(_Arg, {{send, Payload}, #state{type = '2way',
												id = Id,
												fw_q = FQ,
												bw_q = BQ,
												chan = Chan}}, _ReplyTo, _WP) ->
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
	{reply, Response, normal}.

handle_child_forked(_Task, _Child, ModState) ->
	{noreply, ModState}.

handle_child_terminated(_Reason, _Task, _Child, ModState) ->
	{noreply, ModState}.