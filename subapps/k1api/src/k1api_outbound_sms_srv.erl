-module(k1api_outbound_sms_srv).

-behaviour(gen_server).

-export([
	start_link/0,
	send/3
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-include("logging.hrl").
-include("gen_server_spec.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("billy_client/include/billy_client.hrl").

-define(SmsRequestQueue, <<"pmm.k1api.sms_request">>).

-record(state, {
	chan :: pid()
}).

%% ===================================================================
%% API Functions Definitions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(#just_sms_request_dto{}, #k1api_auth_response_dto{}, #credentials{}) -> ok | {error, any()}.
send(OutboundSms, Customer, Credentials) ->
	?log_debug("Got SendSmsRequest", []),
	#outbound_sms{
		address = RawDestAddresses,
		message = Message
	} = OutboundSms,
	#k1api_auth_response_dto{
		billing_type = BillingType
	} = Customer,

	Destinations = oneapi_addr_to_dto(RawDestAddresses),

	{Encoding, Encoded} =
		case gsm0338:from_utf8(Message) of
			{valid, Binary} -> {{text, default}, Binary};
			{invalid, Binary} -> {{text, ucs2}, Binary}
		end,
	NumberOfSymbols = size(Encoded),

	{ok, NumberOfParts} = get_message_parts(NumberOfSymbols, Encoding),
	?log_debug("Encoded message: ~p, Encoding: ~p, Symbols: ~p, Parts: ~p",
		[Encoded, Encoding, NumberOfSymbols, NumberOfParts]),

	case BillingType of
		prepaid ->
			bill_and_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations);
		postpaid ->
			just_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations)
	end.

bill_and_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations) ->
	#k1api_auth_response_dto{
		uuid = CustomerID
	} = Customer,
	#credentials{
		user = UserID
	} = Credentials,

	NumberOfDests = length(Destinations),
	NumberOfMsgs = NumberOfDests * NumberOfParts,

	{ok, SessionID} = k1api_billy_session:get_session_id(),
	case billy_client:reserve(
		SessionID, ?CLIENT_TYPE_ONEAPI, CustomerID, list_to_binary(UserID), ?SERVICE_TYPE_SMS_ON, NumberOfMsgs
	) of
		{accepted, TransID} ->
			?log_debug("Reserve accepted: ~p", [TransID]),
			case just_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations) of
				{ok, RequestIDStr} ->
					commited = billy_client:commit(TransID),
					?log_debug("Commited.", []),
					{ok, RequestIDStr};
				{error, Reason} ->
					?log_debug("Send failed with: ~p", [Reason]),
					rolledback = billy_client:rolledback(TransID),
					?log_debug("Rolledback.", []),
					{error, Reason}
			end;
		{rejected, Reason} ->
			?log_debug("Reserve rejected with: ~p", [Reason]),
			{error, Reason}
	end.

%% funnel encode_batch(Common, Dests, BatchId, GtwId)
just_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations) ->
	#outbound_sms{
		sender_address = RawSenderAddress,
		message = Message,
		notify_url = NotifyURL, % opt
		client_correlator = Correlator, %opt
		callback_data = CallbackData % opt
	} = OutboundSms,
	#k1api_auth_response_dto{
		uuid = CustomerID,
		allowed_sources = AllowedSources,
		default_validity = DefaultValidity,
		no_retry = NoRetry
	} = Customer,
	#credentials{
		user = User
	} = Credentials,

	ReqID = uuid:newid(),
	Params = [
			{just_sms_request_param_dto,<<"registered_delivery">>,{boolean, true}},
			{just_sms_request_param_dto,<<"service_type">>,{string,<<>>}},
			{just_sms_request_param_dto,<<"no_retry">>,{boolean, NoRetry}},
			{just_sms_request_param_dto,<<"validity_period">>,{string, <<"000003000000000R">>}},
			{just_sms_request_param_dto,<<"priority_flag">>,{integer,0}},
			{just_sms_request_param_dto,<<"esm_class">>,{integer,3}},
			{just_sms_request_param_dto,<<"protocol_id">>,{integer,0}},
			{just_sms_request_param_dto, <<"k1api_notify_url">>, {string, NotifyURL}},
			{just_sms_request_param_dto, <<"k1api_callback_data">>, {string, CallbackData}}
			],
	NumberOfDests = length(Destinations),
	GtwID = get_suitable_gtw(Customer, NumberOfDests),
	MessageIDs = get_ids(CustomerID, NumberOfDests, NumberOfParts),
	?log_debug("Message IDs: ~p", [MessageIDs]),
	DTO = #just_sms_request_dto{
		id = ReqID,
		gateway_id = GtwID,
		customer_id = CustomerID,
		client_type = k1api,
		type = regular,
		message = Message,
		encoding = Encoding,
		params = Params,
		source_addr = prepare_source_addr(AllowedSources, RawSenderAddress),
		dest_addrs = {regular, Destinations},
		message_ids = MessageIDs
	},
	?log_debug("Built SmsRequest: ~p", [DTO]),
	ok = k1api_correlator_cache:process(CustomerID, User, Correlator, ReqID),
	case adto:encode(DTO) of
		{ok, Bin} ->
			RequestIDStr = uuid:to_string(ReqID),
			?log_debug("SmsRequest was sucessfully encoded", []),
			ok = publish_sms_request(Bin, RequestIDStr, GtwID),
			{ok, RequestIDStr};
		{error, Error} ->
			{error, Error}
	end.

%% ===================================================================
%% GenServer Callback Functions Definitions
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Channel} = rmql:channel_open(Connection),
	link(Channel),
	ok = rmql:queue_declare(Channel, ?SmsRequestQueue, []),
	{ok, #state{chan = Channel}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(Req, State) ->
    {stop, {unexpected_cast, Req}, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

publish_sms_request(Payload, ReqID, GtwID) ->
    Basic = #'P_basic'{
        content_type = <<"k1apiSmsRequest">>,
        delivery_mode = 2,
        priority = 1,
        message_id = list_to_binary(ReqID)
    },
	{ok, Channel} = gen_server:call(?MODULE, get_channel),
    GtwQueue = re:replace("pmm.just.gateway.%id%", "%id%", uuid:to_string(GtwID), [{return, binary}]),
	?log_debug("Sending message to ~p & ~p through the ~p", [?SmsRequestQueue, GtwQueue, Channel]),
    ok = rmql:basic_publish(Channel, ?SmsRequestQueue, Payload, Basic),
    ok = rmql:basic_publish(Channel, GtwQueue, Payload, Basic).


prepare_source_addr(AllowedSources, RawSenderAddress) ->
	[_, SenderAddress] = binary:split(RawSenderAddress, <<"tel:+">>),
	IsAddrAllowed = lists:filter(fun(AllowedSource) ->
		AllowedSource#addr_dto.addr == SenderAddress andalso
		AllowedSource#addr_dto.ton == 1 andalso
		AllowedSource#addr_dto.npi == 1
		end, AllowedSources),
	case IsAddrAllowed of
		[] ->
			erlang:error(sender_address_not_available);
		[Addr | _] ->
			Addr
	end.

oneapi_addr_to_dto(OneAPIAddresses) when is_list(OneAPIAddresses) ->
	[oneapi_addr_to_dto(Addr) || Addr <- OneAPIAddresses];
oneapi_addr_to_dto(OneAPIAddress) ->
	[_, Address] = binary:split(OneAPIAddress, <<"tel:+">>),
	#addr_dto{
		addr = Address,
		ton = 1,
		npi = 1
	}.

get_suitable_gtw(Customer, NumberOfDests) ->
	#k1api_auth_response_dto{
		default_provider_id = DefaultProviderID,
		providers = Providers,
		networks = Networks
	} = Customer,
	get_suitable_gtw(DefaultProviderID, Networks, Providers, NumberOfDests).
get_suitable_gtw(undefined, _Networks, _Providers, _NumberOfDests) ->
	erlang:error(gtw_choice_not_implemented);
get_suitable_gtw(DefaultProviderID, _Networks, Providers, _NumberOfDests) ->
	[Provider] = lists:filter(fun(Provider) ->
		Provider#provider_dto.id == DefaultProviderID
		end, Providers),
	Provider#provider_dto.gateway.

get_ids(CustomerID, NumberOfDests, Parts) ->
	{ok, IDs} = k1api_db:next_id(CustomerID, NumberOfDests * Parts),
	StringIDs = [integer_to_list(ID) || ID <- IDs],
	{GroupStringIDs, []} = lists:foldl(fun(ID, {Acc, Group}) ->
			case length(Group) + 1 of
				Parts -> {[string:join(lists:reverse([ID | Group]), ":") | Acc], []};
				_ -> {Acc, [ID | Group]}
			end
		end, {[], []}, StringIDs),
	lists:map(fun(ID) -> list_to_binary(ID) end, lists:reverse(GroupStringIDs)).

get_message_parts(Size, {text, default}) when Size =< 160 ->
	{ok, 1};
get_message_parts(Size, {text, default}) ->
	case (Size rem 153) == 0 of
		true -> {ok, trunc(Size/153)};
		false -> {ok, trunc(Size/153) +1}
	end;
get_message_parts(Size, {text, ucs2}) when Size =< 70 ->
	{ok, 1};
get_message_parts(Size, {text, ucs2}) ->
	case (Size rem 67) == 0 of
		true -> {ok, trunc(Size/67)};
		false -> {ok, trunc(Size/67) + 1}
	end.
