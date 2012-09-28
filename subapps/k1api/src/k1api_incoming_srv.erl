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

-include_lib("alley_dto/include/adto.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include("gen_server_spec.hrl").
-include("logging.hrl").

-define(IncomingQueue, <<"pmm.k1api.incoming_sms">>).

-record(state, {
	chan :: pid()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ok = rmql:queue_declare(Chan, ?IncomingQueue, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?IncomingQueue, NoAck),
	{ok, #state{chan = Chan}}.

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{},
			 #amqp_msg{props = Props, payload = Payload}},
			 State = #state{chan = Chan}) ->
	#'P_basic'{
		reply_to = ReplyTo,
		message_id = MsgID,
		content_type = ContentType
	} = Props,
	{ok, DTO} = decode_dto(Payload, ContentType),
	#k1api_sms_notification_request_dto{
		message_id = ID
	} = DTO,
	process_dto(DTO),
	?log_debug("Got DTO: ~p", [DTO]),
	respond_and_ack(ID, MsgID, ReplyTo, Chan),
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
	DTO = #funnel_ack_dto{id = ID},
    {ok, Encoded} =	adto:encode(DTO),
    RespProps = #'P_basic'{
        content_type   = <<"BatchAck">>,
        correlation_id = MsgId,
        message_id     = list_to_binary(uuid:to_string(uuid:newid()))
    },
    rmql:basic_publish(Chan, ReplyTo, Encoded, RespProps).

decode_dto(Bin, <<"OutgoingBatch">>) ->
	adto:decode(#k1api_sms_notification_request_dto{}, Bin);
decode_dto(_Bin, <<"ReceiptBatch">>) ->
	erlang:error(not_implemented).
	%% adto:decode(#k1api_sms_notification_request_dto{}, Bin).

process_dto(DTO = #k1api_sms_notification_request_dto{}) ->
	#k1api_sms_notification_request_dto{
		callback_data = Callback,
		datetime = _DateTime,
		dest_addr = DestAddr,
		message_id = MessageID,
		message = Message,
		sender_addr = SenderAddr,
		notify_url  = URL
	} = DTO,
	InboundSms = #inbound_sms{
		notify_url = URL,
		date_time = {{2012,10,24},{13,45,00}},
		destination_address = DestAddr#addr_dto.addr,
		message_id = uuid:to_string(MessageID),
		message = Message,
		sender_address = SenderAddr#addr_dto.addr,
		callback_data = Callback
	},
	ok = eoneapi:deliver_sms(json, InboundSms).


