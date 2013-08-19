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

-spec authenticate(Credentials :: #credentials{}) ->
	{ok, Customer :: #k1api_auth_response_dto{}} |
	{error, denied} |
	{error, Error :: term()}.
authenticate(Credentials = #credentials{}) ->
	#credentials{
		system_id = CustomerID,
		user_id = UserID,
		password = Pswd } = Credentials,
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
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ok = rmql:queue_declare(Chan, ?K1API_AUTH_RESP_Q, []),
	ok = rmql:queue_declare(Chan, ?K1API_AUTH_REQ_Q, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?K1API_AUTH_RESP_Q, NoAck),
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

handle_info({#'basic.deliver'{},
			 #amqp_msg{props = #'P_basic'{}, payload = Content}},
			 State = #state{
			 	pending_responses = ResponsesList,
				pending_workers = WorkersList}) ->
	?log_debug("Got auth response", []),
	case adto:decode(#k1api_auth_response_dto{}, Content) of
		{ok, AuthResponse = #k1api_auth_response_dto{
				id = CorrelationID }} ->
			?log_debug("AuthResponse was sucessfully decoded [id: ~p]", [CorrelationID]),
			Response = #presponse{id = CorrelationID, timestamp = k1api_lib:get_now(), response = AuthResponse},
			{ok, NRList, NWList} = k1api_lib:process_response(Response, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
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

request_backend_auth(Credentials) ->
	#credentials{
		system_id = CustomerSystemID,
		user_id = UserID,
		password = Password } = Credentials,
 	{ok, Channel} = get_channel(),
	RequestUUID = uuid:unparse(uuid:generate()),
    AuthRequest = #k1api_auth_request_dto{
        id = RequestUUID,
        customer_id = CustomerSystemID,
        user_id = UserID,
        password = Password
    },
	{ok, Payload} = adto:encode(AuthRequest),
    Props = #'P_basic'{},
    ok = rmql:basic_publish(Channel, ?K1API_AUTH_REQ_Q, Payload, Props),
	{ok, RequestUUID}.
