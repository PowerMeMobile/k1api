-module(k1api_delivery_status_srv).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	get/4
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
-include("gen_server_spec.hrl").
-include("logging.hrl").

-define(deliveryStatusRequestQueue, <<"pmm.k1api.delivery_status_request">>).
-define(deliveryStatusResponseQueue, <<"pmm.k1api.delivery_status_response">>).

-record(pworker, {
	id,
	timestamp,
	from
}).

-record(presponse, {
	id,
	timestamp,
	response
}).

-record(state, {
	chan :: pid(),
	reply_to :: binary(),
	pending_workers = [] :: [#pworker{}],
	pending_responses = [] :: [#presponse{}]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(binary(), bitstring(), bitstring(), binary()) -> {ok, [#k1api_sms_status_dto{}]}.
get(CustomerUUID, UserID, SenderAddress, SendSmsRequestId) ->
	{ok, RequestID} = request_backend(CustomerUUID, UserID, SenderAddress, SendSmsRequestId),
	?log_debug("Successfully sent request [~p] to backend", [RequestID]),
	{ok, Response} = get_response(RequestID),
	{ok, Response#k1api_sms_delivery_status_response_dto.statuses}.

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ok = rmql:queue_declare(Chan, ?deliveryStatusResponseQueue, []),
	ok = rmql:queue_declare(Chan, ?deliveryStatusRequestQueue, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?deliveryStatusResponseQueue, NoAck),
	{ok, #state{chan = Chan}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call({get_response, MesID}, From,
					State = #state{
								pending_workers = WList,
								pending_responses = RList}) ->
	Worker = #pworker{id = MesID, from = From, timestamp = get_now()},
	{ok, NRList, NWList} = process_worker_request(Worker, RList, WList),
	{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{}, #amqp_msg{payload = Content}},
			 State = #state{
			 	pending_responses = ResponsesList,
				pending_workers = WorkersList}) ->
	?log_debug("Got sms delivery status response", []),
	case adto:decode(#k1api_sms_delivery_status_response_dto{}, Content) of
		{ok, Response = #k1api_sms_delivery_status_response_dto{
				id = CorrelationID }} ->
			?log_debug("Response was sucessfully decoded [id: ~p]", [CorrelationID]),
			NewPendingResponse = #presponse{id = CorrelationID, timestamp = get_now(), response = Response},
			{ok, NRList, NWList} = process_response(NewPendingResponse, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			?log_error("Failed To Decode Response Due To ~p : ~p", [Error, Content]),
			{noreply, State}
	end;

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

get_channel() ->
	gen_server:call(?MODULE, get_channel).

get_response(RequestUUID) ->
	gen_server:call(?MODULE, {get_response, RequestUUID}).

request_backend(CustomerUUID, UserID, SenderAddress, SendSmsRequestId) ->
 	{ok, Channel} = get_channel(),
	RequestUUID = uuid:newid(),
	DeliveryStatusReqDTO = #k1api_sms_delivery_status_request_dto{
		id = RequestUUID,
		customer_id = CustomerUUID,
		user_id = UserID,
		sms_request_id = SendSmsRequestId,
		address = convert_addr_to_dto(SenderAddress)
	},
	?log_debug("DeliveryStatusReqDTO: ~p", [DeliveryStatusReqDTO]),
	{ok, Payload} = adto:encode(DeliveryStatusReqDTO),
    ok = rmql:basic_publish(Channel, ?deliveryStatusRequestQueue, Payload, #'P_basic'{}),
	{ok, RequestUUID}.

convert_addr_to_dto(SenderAddress) ->
	#addr_dto{
		addr = SenderAddress,
		ton = 1,
		npi = 1
	}.

process_response(PResponse = #presponse{id = ID, response = Response}, RList, WList) ->
		case lists:keytake(ID, #pworker.id, WList) of
		{value, #pworker{from = From}, RestWorkerList} ->
			gen_server:reply(From, {ok, Response}),
			{ok, purge(RList), purge(RestWorkerList)};
		false ->
			{ok, [PResponse] ++ purge(RList), purge(WList)}
	end.

process_worker_request(Worker = #pworker{id = ItemID, from = From}, RList, WList) ->
	case lists:keytake(ItemID, #presponse.id, RList) of
		{value, #presponse{response = Response}, RestRespList} ->
			gen_server:reply(From, {ok, Response}),
			{ok, purge(RestRespList), purge(WList)};
		false ->
			{ok, purge(RList), [Worker] ++ purge(WList)}
	end.

purge(List) ->
	{ok, ExpirationInterval} = application:get_env(k1api, request_timeout),
	purge(List, [], get_now() - ExpirationInterval).

purge([], Acc, _Now) -> Acc;
purge([#pworker{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([#presponse{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([Item | RestList], Acc, Now) ->
	purge(RestList, [Item | Acc], Now).

get_now() ->
	 calendar:datetime_to_gregorian_seconds(calendar:local_time()).
