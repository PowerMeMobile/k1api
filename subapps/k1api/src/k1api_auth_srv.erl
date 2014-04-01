-module(k1api_auth_srv).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	authenticate/1
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

-spec authenticate(#credentials{}) ->
	{ok, #k1api_auth_response_customer_dto{}} |
	{error, denied} |
	{error, term()}.
authenticate(Credentials = #credentials{
    customer_id = CustomerID,
	user_id = UserID,
	password = Pswd
}) ->
	case k1api_auth_cache:fetch({CustomerID, UserID, Pswd}) of
		{ok, Customer} ->
			?log_debug("User found in cache", []),
			{ok, Customer};
		not_found ->
			?log_debug("User NOT found in cache", []),
			{ok, RequestID} = request_backend_auth(Credentials),
			?log_debug("Sent auth request [id: ~p]", [RequestID]),
			{ok, Customer} = get_auth_response(RequestID),
			ok = k1api_auth_cache:store({CustomerID, UserID, Pswd}, Customer),
			?log_debug("Got sucessful auth response", []),
			{ok, Customer}
	end.

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
    {ok, AuthReqQueue} = application:get_env(?APP, auth_req_queue),
    {ok, AuthRespQueue} = application:get_env(?APP, auth_resp_queue),
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ok = rmql:queue_declare(Chan, AuthReqQueue, []),
	ok = rmql:queue_declare(Chan, AuthRespQueue, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, AuthRespQueue, NoAck),
	{ok, #state{chan = Chan}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call({get_response, MesID}, From, State = #state{
    pending_workers = WList,
	pending_responses = RList
}) ->
	Worker = #pworker{id = MesID, from = From, timestamp = k1api_lib:get_now()},
	{ok, NRList, NWList} = k1api_lib:process_worker_request(Worker, RList, WList),
	{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info({#'basic.deliver'{}, AmqpMsg = #amqp_msg{}}, State = #state{}) ->
	?log_debug("Got auth response", []),
    Content = AmqpMsg#amqp_msg.payload,
    ResponsesList = State#state.pending_responses,
    WorkersList = State#state.pending_workers,
	case adto:decode(#k1api_auth_response_dto{}, Content) of
		{ok, AuthResp = #k1api_auth_response_dto{}} ->
            CorrelationID = AuthResp#k1api_auth_response_dto.id,
            ?log_debug("Got auth response: ~p", [AuthResp]),
            Response = #presponse{
                id = CorrelationID,
                timestamp = k1api_lib:get_now(),
                response = AuthResp
            },
			{ok, NRList, NWList} =
                k1api_lib:process_response(Response, ResponsesList, WorkersList),
			{noreply, State#state{
                pending_workers = NWList,
                pending_responses = NRList
            }};
		{error, Error} ->
			?log_error("Failed To Decode Auth Response Due To ~p : ~p", [Error, Content]),
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

get_auth_response(RequestUUID) ->
	gen_server:call(?MODULE, {get_response, RequestUUID}).

request_backend_auth(#credentials{
    customer_id = CustomerID,
	user_id = UserID,
	password = Password
}) ->
 	{ok, Channel} = get_channel(),
	RequestUUID = uuid:unparse(uuid:generate()),
    AuthRequest = #k1api_auth_request_dto{
        id = RequestUUID,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password
    },
    ?log_debug("Sending auth request: ~p", [AuthRequest]),
	{ok, Payload} = adto:encode(AuthRequest),
    {ok, AuthReqQueue} = application:get_env(?APP, auth_req_queue),
    {ok, AuthRespQueue} = application:get_env(?APP, auth_resp_queue),
    Props = #'P_basic'{reply_to = AuthRespQueue},
    ok = rmql:basic_publish(Channel, AuthReqQueue, Payload, Props),
	{ok, RequestUUID}.
