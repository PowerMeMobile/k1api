-module(k1api_incoming_srv).

-behaviour(gen_server).

-export([
	start_link/0
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-include("FunnelAsn.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include("gen_server_spec.hrl").

-record(state, {
	chan :: pid()
}).

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Chan = k1api_amqp_pool:open_channel(),
	link(Chan),

	% declare incoming queue
	{ok, IncomingQ} = application:get_env(incoming_queue),
	ok = k1api_amqp_funs:queue_declare(Chan, IncomingQ),

	NoAck = true,
	{ok, _ConsumerTag} = k1api_amqp_funs:basic_consume(Chan, IncomingQ, NoAck),
	{ok, #state{chan = Chan}}.

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{},
			 #amqp_msg{props = Props, payload = Payload}},
			 State = #state{chan = Chan}) ->
	%% imported from fun_batch_runner.erl
    Gunzipped =
        case Props#'P_basic'.content_encoding of
            <<"gzip">> -> zlib:gunzip(Payload);
            _          -> Payload
        end,
    Type =
        case Props#'P_basic'.content_type of
            <<"OutgoingBatch">> -> generic;
            <<"ReceiptBatch">>  -> receipts
        end,
    {ID, AllItems} =
        case Type of
            generic ->
                {ok, Str} = 'FunnelAsn':outgoing_batch_id(Gunzipped),
                {ok, #'OutgoingBatch'{messages = {'OutgoingBatch_messages', Its}}} =
                    'FunnelAsn':outgoing_batch_messages(Gunzipped),
                {Str, Its};
            receipts ->
                {ok, Str} = 'FunnelAsn':receipt_batch_id(Gunzipped),
                {ok, #'ReceiptBatch'{receipts = {'ReceiptBatch_receipts', Its}}} =
                    'FunnelAsn':receipt_batch_messages(Gunzipped),
                {Str, Its}
        end,
	?log_debug("ID: ~p, AllItems: ~p", [ID, AllItems]),
    MsgId   = Props#'P_basic'.message_id,
    ReplyTo = Props#'P_basic'.reply_to,
	respond_and_ack(ID, MsgId, ReplyTo, Chan),
	{noreply, State};

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------------------------------------------------------------------
% Internal Function Definitions
% ------------------------------------------------------------------

respond_and_ack(ID, MsgId, ReplyTo, Chan) ->
    {ok, Encoded} =
        'FunnelAsn':encode('BatchAck', #'BatchAck'{batchId = ID}),
    RespPayload = list_to_binary(Encoded),
    RespProps = #'P_basic'{
        content_type   = <<"BatchAck">>,
        correlation_id = MsgId,
        message_id     = k1api_uuid:bin_id()
    },
    k1api_amqp_funs:basic_publish(Chan, ReplyTo, RespPayload, RespProps).
