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

init(#peer_spec{id = Id, fw_q = FQ, bw_q = BQ, type = Type, qprops = Props}) ->
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, FQ, Props),
	gproc:add_local_name({?MODULE, Id}),
	{ok, #state{id = Id, chan = Chan, fw_q = FQ, bw_q = BQ, type = Type}}.

handle_call(Msg = {send, _Payload, _Props}, _From, State = #state{}) ->
	% ?log_debug("Msg: ~p", [Msg]),
	{fork, {Msg, State}, State#state{}};
handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(Msg = {send, _Payload, _Props}, State = #state{}) ->
	% ?log_debug("Msg: ~p", [Msg]),
	{fork, {Msg, State}, State};
handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_fork_call(_Arg, {{send, Payload, Props}, #state{
													type = forward,
													fw_q = FQ,
													chan = Chan}}, _ReplyTo, _WP) ->
	% ?log_debug("handle_fork_call", []),
	BasicProps = prepare_basic_props(Props),
	Result = oabc_amqp:basic_publish(Chan, FQ, Payload, BasicProps),
	% ?log_debug("basic_publish ok", []),
	{reply, Result, normal};

handle_fork_call(_Arg, {{send, Payload, Props}, #state{
													type = '2way',
													id = Id,
													fw_q = FQ,
													bw_q = BQ,
													chan = Chan}}, _ReplyTo, _WP) ->
	BasicProps = prepare_basic_props([{reply_to, BQ}] ++ Props),
	ok = oabc_amqp:basic_publish(Chan, FQ, Payload, BasicProps),
	% ?log_debug("basic_publish ok", []),
	Response = oabc_consumer_srv:get_response(Id, BasicProps#'P_basic'.message_id),
	{reply, Response, normal};
handle_fork_call(_Arg, _Msg, _ReplyTo, _WP) ->
	{error, unexpected_call}.


handle_fork_cast(_Arg, {{send, Payload, Props}, #state{ %% cast only for forward
													type = forward,
													fw_q = FQ,
													chan = Chan}}, _WP) ->
	% ?log_debug("handle_fork_cast", []),
	BasicProps = prepare_basic_props(Props),
	oabc_amqp:basic_publish(Chan, FQ, Payload, BasicProps),
	% ?log_debug("basic_publish ok", []),
	{noreply, normal};
handle_fork_cast(_Arg, _Msg, _WP) ->
	{error, unexpected_cast}.

handle_child_forked(_Task, _Child, ModState) ->
	{noreply, ModState}.

handle_child_terminated(_Reason, _Task, _Child, ModState) ->
	{noreply, ModState}.

%% Internal functions

prepare_basic_props(Props) ->
	{ok, AppId} = application:get_env(app_id),
	MsgId =
		case proplists:get_value(message_id, Props) of
			undefined -> oabc_uuid:newid();
			SomeId -> SomeId
		end,
	#'P_basic'{
		message_id = MsgId,
		correlation_id = proplists:get_value(correlation_id, Props),
		content_type = proplists:get_value(content_type, Props),
		content_encoding = proplists:get_value(content_encoding, Props),
		delivery_mode = proplists:get_value(delivery_mode, Props, 2),
		reply_to = proplists:get_value(reply_to, Props),
		expiration = proplists:get_value(expiration, Props),
		timestamp = proplists:get_value(timestamp, Props),
		app_id = AppId
		% headers,priority,type,user_id,cluster_id
		}.