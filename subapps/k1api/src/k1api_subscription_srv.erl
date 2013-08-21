-module(k1api_subscription_srv).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	subscribe_incoming_sms/2,
	unsubscribe_incoming_sms/2,
	subscribe_receipts/2,
	unsubscribe_receipts/2
]).

%% GenServer Callbacks
-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("queue_fabric/include/queue_fabric.hrl").
-include("gen_server_spec.hrl").
-include("application.hrl").
-include("logging.hrl").

-record(state, {
	chan 					:: pid(),
	reply_to 				:: binary(),
	pending_workers = [] 	:: [#pworker{}],
	pending_responses = [] 	:: [#presponse{}]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec subscribe_incoming_sms(binary(), binary()) -> {ok, binary()}.
subscribe_incoming_sms(RequestID, Payload) ->
	process_request(RequestID, Payload, <<"SubscribeIncomingSms">>).

-spec unsubscribe_incoming_sms(binary(), binary()) -> {ok, ok}.
unsubscribe_incoming_sms(RequestID, Payload) ->
	process_request(RequestID, Payload, <<"UnsubscribeIncomingSms">>).

-spec subscribe_receipts(binary(), binary()) -> {ok, binary()}.
subscribe_receipts(RequestID, Payload) ->
	process_request(RequestID, Payload, <<"SubscribeReceipts">>).

-spec unsubscribe_receipts(binary(), binary()) -> {ok, ok}.
unsubscribe_receipts(RequestID, Payload) ->
	process_request(RequestID, Payload, <<"UnsubscribeReceipts">>).

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ok = rmql:queue_declare(Chan, ?K1API_SUBSCRIPTION_REQ_Q, []),
	ok = rmql:queue_declare(Chan, ?K1API_SUBSCRIPTION_RESP_Q, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?K1API_SUBSCRIPTION_RESP_Q, NoAck),
	{ok, #state{chan = Chan}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call({get_response, MesID}, From,
					State = #state{
								pending_workers = WList,
								pending_responses = RList}) ->
	Worker = #pworker{id = MesID, from = From, timestamp = k1api_lib:get_now()},
	{ok, NRList, NWList} = k1api_lib:process_worker_request(Worker, RList, WList),
	{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{}, AMQPMessage}, State = #state{}) ->
	#amqp_msg{props = Props, payload = Content} = AMQPMessage,
	decode_incoming(Props#'P_basic'.content_type, Content, State);

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

process_request(RequestID, Payload, ContentType) ->
	ok = request_backend(RequestID, Payload, ContentType),
	?log_debug("Sent unsubscribe incoming sms request", []),
	get_response(RequestID).

request_backend(_RequestID, Payload, ContentType) ->
 	{ok, Channel} = get_channel(),
	Props = [
		{content_type, ContentType},
		{reply_to, ?K1API_SUBSCRIPTION_RESP_Q}
	],
    ok = rmql:basic_publish(Channel, ?K1API_SUBSCRIPTION_REQ_Q, Payload, Props).

get_response(RequestUUID) ->
	gen_server:call(?MODULE, {get_response, RequestUUID}).

decode_incoming(<<"SubscribeIncomingSms">>, Content, State) ->
	#state{
	 	pending_responses = ResponsesList,
		pending_workers = WorkersList} = State,
	case adto:decode(#k1api_subscribe_incoming_sms_response_dto{}, Content) of
		{ok, #k1api_subscribe_incoming_sms_response_dto{
				id = CorrelationID,
				subscription_id = SubscriptionID }} ->
			?log_debug("Got subscribe incoming sms response", []),
			?log_debug("Response was sucessfully decoded [id: ~p]", [CorrelationID]),
			NewPendingResponse = #presponse{id = CorrelationID, timestamp = k1api_lib:get_now(), response = SubscriptionID},
			{ok, NRList, NWList} = k1api_lib:process_response(NewPendingResponse, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			?log_error("Failed To Decode Response Due To ~p : ~p", [Error, Content]),
			{noreply, State}
	end;
decode_incoming(<<"UnsubscribeIncomingSms">>, Content, State) ->
	#state{
	 	pending_responses = ResponsesList,
		pending_workers = WorkersList} = State,
	case adto:decode(#k1api_unsubscribe_incoming_sms_response_dto{}, Content) of
		{ok, #k1api_unsubscribe_incoming_sms_response_dto{
				id = CorrelationID }} ->
			?log_debug("Got unsubscribe incoming sms response", []),
			?log_debug("Response was sucessfully decoded [id: ~p]", [CorrelationID]),
			NewPendingResponse = #presponse{id = CorrelationID, timestamp = k1api_lib:get_now(), response = ok},
			{ok, NRList, NWList} = k1api_lib:process_response(NewPendingResponse, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			?log_error("Failed To Decode Response Due To ~p : ~p", [Error, Content]),
			{noreply, State}
	end;
decode_incoming(<<"SubscribeReceipts">>, Content, State) ->
	#state{
	 	pending_responses = ResponsesList,
		pending_workers = WorkersList} = State,
	case adto:decode(#k1api_subscribe_sms_receipts_response_dto{}, Content) of
		{ok, #k1api_subscribe_sms_receipts_response_dto{
				id = CorrelationID }} ->
			?log_debug("Got subscribe sms receipts  response", []),
			?log_debug("Response was sucessfully decoded [id: ~p]", [CorrelationID]),
			NewPendingResponse = #presponse{id = CorrelationID, timestamp = k1api_lib:get_now(), response = CorrelationID},
			{ok, NRList, NWList} = k1api_lib:process_response(NewPendingResponse, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			?log_error("Failed To Decode Response Due To ~p : ~p", [Error, Content]),
			{noreply, State}
	end;
decode_incoming(<<"UnsubscribeReceipts">>, Content, State) ->
	#state{
	 	pending_responses = ResponsesList,
		pending_workers = WorkersList} = State,
	case adto:decode(#k1api_unsubscribe_sms_receipts_response_dto{}, Content) of
		{ok, #k1api_unsubscribe_sms_receipts_response_dto{
				id = CorrelationID }} ->
			?log_debug("Got unsubscribe sms receipts  response", []),
			?log_debug("Response was sucessfully decoded [id: ~p]", [CorrelationID]),
			NewPendingResponse = #presponse{id = CorrelationID, timestamp = k1api_lib:get_now(), response = ok},
			{ok, NRList, NWList} = k1api_lib:process_response(NewPendingResponse, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			?log_error("Failed To Decode Response Due To ~p : ~p", [Error, Content]),
			{noreply, State}
	end;
decode_incoming(ContentType, _Content, State) ->
	?log_error("Got unexpected message type: ~p", [ContentType]),
	{noreply, State}.

get_channel() ->
	gen_server:call(?MODULE, get_channel).
