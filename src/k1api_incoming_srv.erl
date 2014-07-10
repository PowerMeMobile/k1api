-module(k1api_incoming_srv).

-behaviour(gen_server).

%% API
-export([
    start_link/0
]).

%% GenServer Callback Exports
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
-include("eoneapi.hrl").
-include("gen_server_spec.hrl").
-include("application.hrl").
-include("logging.hrl").

-record(state, {
    chan :: pid()
}).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% GenServer Callback Functions
%% ===================================================================

init([]) ->
    {ok, IncomingQueue} = application:get_env(?APP, incoming_sms_queue),
    {ok, Connection} = rmql:connection_start(),
    {ok, Chan} = rmql:channel_open(Connection),
    link(Chan),
    ok = rmql:queue_declare(Chan, IncomingQueue, []),
    NoAck = true,
    {ok, _ConsumerTag} = rmql:basic_consume(Chan, IncomingQueue, NoAck),
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
    ?log_debug("Got DTO: ~p", [DTO]),
    case process_dto(DTO) of
        {ok, ID} ->
            respond_and_ack(ID, MsgID, ReplyTo, Chan);
        noreply ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

respond_and_ack(ID, MsgId, ReplyTo, Chan) ->
    DTO = #funnel_ack_dto{id = ID},
    {ok, Encoded} =    adto:encode(DTO),
    RespProps = #'P_basic'{
        content_type   = <<"BatchAck">>,
        correlation_id = MsgId,
        message_id     = uuid:unparse(uuid:generate())
    },
    rmql:basic_publish(Chan, ReplyTo, Encoded, RespProps).

decode_dto(Bin, <<"OutgoingBatch">>) ->
    adto:decode(#k1api_sms_notification_request_dto{}, Bin);
decode_dto(Bin, <<"ReceiptBatch">>) ->
    adto:decode(#k1api_sms_delivery_receipt_notification_dto{}, Bin).

process_dto(DTO = #k1api_sms_notification_request_dto{}) ->
    ?log_debug("Got InboundSms: ~p", [DTO]),
    #k1api_sms_notification_request_dto{
        callback_data = Callback,
        datetime = {MegaSecs, Secs, _MicroSecs},
        dest_addr = DestAddr,
        message_id = MessageID,
        message = Message,
        sender_addr = SenderAddr,
        notify_url  = URL
    } = DTO,
    UnixEpochDateTime = MegaSecs * 1000000 + Secs,
    InboundSms = #inbound_sms{
        notify_url = URL,
        date_time = ac_datetime:unixepoch_to_datetime(UnixEpochDateTime),
        dest_addr = DestAddr#addr.addr,
        message_id = MessageID,
        message = Message,
        sender_addr = SenderAddr#addr.addr,
        callback = Callback
    },
    case eoneapi:deliver_sms(InboundSms) of
        {ok, _} -> {ok, MessageID};
        {error, _Error} -> noreply
    end;
process_dto(DTO = #k1api_sms_delivery_receipt_notification_dto{}) ->
    ?log_debug("Got Receipt: ~p", [DTO]),
    #k1api_sms_delivery_receipt_notification_dto{
        id = ItemID,
        dest_addr = DestAddr,
        status = MessageState,
        callback_data = CallbackData,
        url = NotifyURL
    } = DTO,
    Receipt = #delivery_receipt{
        notify_url = NotifyURL,
        callback = CallbackData,
        dest_addr = DestAddr#addr.addr,
        status = MessageState
    },
    case eoneapi:deliver_sms_status(Receipt) of
        {ok, _} -> {ok, ItemID};
        {error, _Error} -> noreply
    end.
