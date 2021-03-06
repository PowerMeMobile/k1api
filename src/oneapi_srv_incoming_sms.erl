-module(oneapi_srv_incoming_sms).

-behaviour(gen_server).

%% API
-export([
    start_link/0
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

-include("application.hrl").
-include("oneapi_srv.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

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
%% gen_server callbacks
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
    {ok, Encoded} = adto:encode(DTO),
    RespProps = #'P_basic'{
        content_type   = <<"BatchAck">>,
        correlation_id = MsgId,
        message_id     = uuid:unparse(uuid:generate())
    },
    rmql:basic_publish(Chan, ReplyTo, Encoded, RespProps).

decode_dto(Bin, <<"OutgoingBatch">>) ->
    adto:decode(#incoming_sms_notification_v1{}, Bin);
decode_dto(Bin, <<"ReceiptBatch">>) ->
    adto:decode(#sms_receipt_notification_v1{}, Bin).

process_dto(DTO = #incoming_sms_notification_v1{}) ->
    ?log_debug("Got InboundSms: ~p", [DTO]),
    #incoming_sms_notification_v1{
        callback_data = CallbackData,
        datetime = Datetime,
        dst_addr = DestAddr,
        message_id = MessageID,
        message = Message,
        sender_addr = SenderAddr,
        notify_url  = URL
    } = DTO,
    InboundSms = #inbound_sms{
        notify_url = URL,
        datetime = Datetime,
        dest_addr = DestAddr#addr.addr,
        message_id = MessageID,
        message = Message,
        sender_addr = SenderAddr#addr.addr,
        callback_data = CallbackData
    },
    case oneapi_srv_protocol:deliver_sms(InboundSms) of
        {ok, _} ->
            {ok, MessageID};
        {error, _Error} ->
            noreply
    end;
process_dto(DTO = #sms_receipt_notification_v1{}) ->
    ?log_debug("Got Receipt: ~p", [DTO]),
    #sms_receipt_notification_v1{
        id = ItemID,
        url = NotifyURL,
        callback_data = CallbackData,
        dst_addr = DestAddr,
        status = Status
    } = DTO,
    Receipt = #delivery_receipt{
        notify_url = NotifyURL,
        callback_data = CallbackData,
        dest_addr = DestAddr#addr.addr,
        status = oneapi_srv_utils:translate_status_name(Status)
    },
    case oneapi_srv_protocol:deliver_sms_status(Receipt) of
        {ok, _} ->
            {ok, ItemID};
        {error, _Error} ->
            noreply
    end.
