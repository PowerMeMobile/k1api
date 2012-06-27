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

-include_lib("k1api_proto/include/FunnelAsn.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include("gen_server_spec.hrl").

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

-spec authenticate(Credentials :: #credentials{}) -> {ok, Customer :: #'Customer'{}} | {error, denied} | {error, Error :: term()}.
authenticate(#credentials{
						system_id = CustSysID,
						user = UserID,
						password = Password,
						type = _Type
						}) ->
	case k1api_cache:fetch({CustSysID, UserID, Password}) of
		{ok, Customer = #'Customer'{}} ->
			?log_debug("Customer in cache: ~p", [Customer]),
			{ok, Customer};
		{error, not_found} ->
			?log_debug("Customer not found in cache. Send auth req.", []),
	   		{ok, Chan} = get_channel(),
			Type = transceiver,
			UUID = k1api_uuid:newid(),
			request_backend_auth(Chan, UUID, CustSysID, UserID, Password, Type),
			Response = get_auth_response(UUID),
			?log_debug("Auth response: ~p", [Response]),
			#'BindResponse'{ result = {customer, Customer}} = Response,
			{ok, Customer}
	end.

get_channel() ->
	gen_server:call(?MODULE, get_channel, 5000).

get_auth_response(UUID) ->
	gen_server:call(?MODULE, {get_response, UUID}, 5000).

request_backend_auth(Chan, UUID, CustomerId, UserId, Password, Type) ->
	Timeout = 5000,
	Addr = "",
    Now = k1api_time:milliseconds(),
    Then = Now + Timeout,
    Timestamp = #'PreciseTime'{time = k1api_time:utc_str(k1api_time:milliseconds_to_now(Now)),
                               milliseconds = Now rem 1000},
    Expiration = #'PreciseTime'{time = k1api_time:utc_str(k1api_time:milliseconds_to_now(Then)),
                                milliseconds = Then rem 1000},
    BindRequest = #'BindRequest'{
        connectionId = UUID,
        remoteIp     = Addr,
        customerId   = CustomerId,
        userId       = UserId,
        password     = Password,
        type         = Type,
        isCached     = false,
        timestamp    = Timestamp,
        expiration   = Expiration
    },
    {ok, Encoded} = 'FunnelAsn':encode('BindRequest', BindRequest),
    Payload = list_to_binary(Encoded),
	{ok, AuthRequestQ} = application:get_env(k1api, auth_req_q),
	{ok, AuthReplyQ} = application:get_env(k1api, auth_resp_q),
    Props = #'P_basic'{
        content_type = <<"BindRequest">>,
        message_id   = list_to_binary(UUID), %% uuid:unparse(uuid:generate()),
        reply_to     = AuthReplyQ
    },
    k1api_amqp_funs:basic_publish(Chan, AuthRequestQ, Payload, Props).

%% GenServer Callback Functions Definitions

init([]) ->
	Chan = k1api_amqp_pool:open_channel(),
	link(Chan),

	% declare reply_to queue
	{ok, ReplyTo} = application:get_env(auth_resp_q),
	ok = k1api_amqp_funs:queue_declare(Chan, ReplyTo),

	% declare auth request queue
	{ok, RequestQ} = application:get_env(auth_req_q),
	ok = k1api_amqp_funs:queue_declare(Chan, RequestQ),

	NoAck = true,
	{ok, _ConsumerTag} = k1api_amqp_funs:basic_consume(Chan, ReplyTo, NoAck),
	{ok, #state{chan = Chan, reply_to = ReplyTo}}.

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
			 	pending_responses = RList,
				pending_workers = WList}) ->
	case 'FunnelAsn':decode('BindResponse', Content) of
		{ok, BindResponse = #'BindResponse'{
				connectionId = CorrelationID
						}} ->
			?log_debug("BindResponse: ~p", [BindResponse]),
			Response = #presponse{id = CorrelationID, timestamp = get_now(), response = BindResponse},
			{ok, NRList, NWList} =
				process_response(Response, RList, WList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, AsnErr} ->
			?log_error("Failed to decode 'BatchAck' due to ~p : ~p", [AsnErr, Content]),
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
