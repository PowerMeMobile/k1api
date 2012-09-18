-module(k1api_auth_srv).

-behaviour(gen_server).

-export([
	start_link/0,
	authenticate/1
	]).

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

-define(AuthRequestQueue, <<"pmm.k1api.auth_request">>).
-define(AuthResponseQueue, <<"pmm.k1api.auth_response">>).

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

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec authenticate(Credentials :: #credentials{}) ->
	{ok, Customer :: #funnel_auth_response_customer_dto{}} |
	{error, denied} |
	{error, Error :: term()}.
authenticate(Credentials = #credentials{}) ->
	?log_debug("Customer not found in cache. Send auth request to Kelly.", []),
	{ok, RequestID} = request_backend_auth(Credentials),
	?log_debug("Sent auth request [id: ~p]", [RequestID]),
	case get_auth_response(RequestID) of
		#funnel_auth_response_dto{result = {customer, Customer}} ->
			?log_debug("Got sucessful auth response", []),
			{ok, Customer};
		#funnel_auth_response_dto{result = {error, Error}} ->
			?log_debug("Got error auth response", []),
			{error, Error}
	end.

	%% case k1api_cache:fetch({CustSysID, UserID, Password}) of
	%% 	{ok, Customer = #funnel_auth_response_customer_dto{}} ->
	%% 		?log_debug("Customer in cache: ~p", [Customer]),
	%% 		{ok, Customer};
	%% 	{error, not_found} ->
	%% 		?log_debug("Customer not found in cache. Send auth req.", []),
	%%    		{ok, Chan} = get_channel(),
	%% 		Type = transceiver,
	%% 		UUID = uuid:newid(),
	%% 		request_backend_auth(Chan, UUID, CustSysID, UserID, Password, Type),
	%% 		Response = get_auth_response(UUID),
	%% 		?log_debug("Auth response: ~p", [Response]),
	%% 		#funnel_auth_response_dto{ result = {customer, Customer}} = Response,
	%% 		{ok, Customer}
	%% end.

get_channel() ->
	gen_server:call(?MODULE, get_channel, 5000).

get_auth_response(RequestUUID) ->
	gen_server:call(?MODULE, {get_response, RequestUUID}, 5000).

request_backend_auth(Credentials) ->
	#credentials{
		system_id = CustomerSystemID,
		user = UserID,
		password = Password,
		type = _Type } = Credentials,
 	{ok, Channel} = get_channel(),
	Timeout = 5000,
	RequestUUID = uuid:newid(),
    Now = k1api_time:milliseconds(),
    Then = Now + Timeout,
    Timestamp = #precise_time_dto{time = list_to_binary(k1api_time:utc_str(k1api_time:milliseconds_to_now(Now))),
                               milliseconds = Now rem 1000},
    Expiration = #precise_time_dto{time = list_to_binary(k1api_time:utc_str(k1api_time:milliseconds_to_now(Then))),
                                milliseconds = Then rem 1000},
    AuthRequest = #funnel_auth_request_dto{
        connection_id = RequestUUID,
        ip = <<"">>,
        customer_id = list_to_binary(CustomerSystemID),
        user_id = list_to_binary(UserID),
        password = list_to_binary(Password),
        type = transceiver,
        is_cached = false,
        timestamp = Timestamp,
        expiration = Expiration
    },
	{ok, Payload} = adto:encode(AuthRequest),
    Props = #'P_basic'{
        %% content_type = <<"OneAPIAuthRequest">>,
        %% message_id   = RequestUUID
    },
    ok = rmql:basic_publish(Channel, ?AuthRequestQueue, Payload, Props),
	{ok, RequestUUID}.

%% GenServer Callback Functions Definitions

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	ok = rmql:queue_declare(Chan, ?AuthResponseQueue, [{durable, false}]),
	ok = rmql:queue_declare(Chan, ?AuthRequestQueue, [{durable, false}]),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?AuthResponseQueue, NoAck),
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

handle_info({#'basic.deliver'{},
			 #amqp_msg{props = #'P_basic'{}, payload = Content}},
			 State = #state{
			 	pending_responses = ResponsesList,
				pending_workers = WorkersList}) ->
	?log_debug("Got auth response", []),
	case adto:decode(#funnel_auth_response_dto{}, Content) of
		{ok, AuthResponse = #funnel_auth_response_dto{
				connection_id = CorrelationID }} ->
			?log_debug("AuthResponse was sucessfully decoded [id: ~p]", [CorrelationID]),
			Response = #presponse{id = CorrelationID, timestamp = get_now(), response = AuthResponse},
			{ok, NRList, NWList} = process_response(Response, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			?log_error("Failed To Decode Auth Response Due To ~P : ~P", [Error, Content]),
			{noreply, State}
	end;

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------------------------------------------------------------------
% Internal Function Definitions
% ------------------------------------------------------------------

process_response(PResponse = #presponse{id = ID, response = Response}, RList, WList) ->
		case lists:keytake(ID, #pworker.id, WList) of
		{value, #pworker{from = From}, RestWorkerList} ->
			gen_server:reply(From, Response),
			{ok, purge(RList), purge(RestWorkerList)};
		false ->
			{ok, [PResponse] ++ purge(RList), purge(WList)}
	end.

process_worker_request(Worker = #pworker{id = ItemID, from = From}, RList, WList) ->
	case lists:keytake(ItemID, #presponse.id, RList) of
		{value, #presponse{}, RestRespList} ->
			gen_server:reply(From, ok),
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

%% prepare_basic_props(Props) ->
%% 	#'P_basic'{
%% 		message_id = proplists:get_value(message_id, Props),
%% 		correlation_id = proplists:get_value(correlation_id, Props),
%% 		content_type = proplists:get_value(content_type, Props),
%% 		content_encoding = proplists:get_value(content_encoding, Props),
%% 		% delivery_mode = proplists:get_value(delivery_mode, Props, 2),
%% 		reply_to = proplists:get_value(reply_to, Props),
%% 		expiration = proplists:get_value(expiration, Props),
%% 		timestamp = proplists:get_value(timestamp, Props),
%% 		app_id = <<"kelly">>
%% 		% headers,priority,type,user_id,cluster_id
%% 		}.
