-module(k1api_outbound_sms_srv).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	send/3
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

-include("logging.hrl").
-include("gen_server_spec.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("billy_client/include/billy_client.hrl").
-include_lib("queue_fabric/include/queue_fabric.hrl").

-define(just_sms_request_param(Name, Param),
	apply(fun
		(undefined) ->
			[];
		(Str) when is_binary(Str) ; is_list(Str) ->
			{just_sms_request_param_dto, Name, {string, Str}};
		(Bool) when Bool =:= true ; Bool =:= false ->
			{just_sms_request_param_dto, Name, {boolean, Bool}};
		(Int) when is_integer(Int) ->
			{just_sms_request_param_dto, Name, {integer, Int}}
	end, [Param])).

-record(state, {
	chan :: pid()
}).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(#just_sms_request_dto{}, #k1api_auth_response_dto{}, #credentials{}) -> ok | {error, any()}.
send(OutboundSms, Customer, Credentials) ->
	?log_debug("Got SendSmsRequest", []),
	#outbound_sms{
		dest_addr = RawDestAddresses,
		message = Message
	} = OutboundSms,
	#k1api_auth_response_dto{
		billing_type = BillingType
	} = Customer,

	Destinations = addr_to_dto(RawDestAddresses),

	{Encoding, Encoded} =
		case gsm0338:from_utf8(Message) of
			{valid, Binary} -> {default, Binary};
			{invalid, Binary} -> {ucs2, Binary}
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

%% ===================================================================
%% GenServer Callback Functions Definitions
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Channel} = rmql:channel_open(Connection),
	link(Channel),
	ok = rmql:queue_declare(Channel, ?K1API_SMS_REQ_Q, []),
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

bill_and_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations) ->
	#k1api_auth_response_dto{
		customer_uuid = CustomerID
	} = Customer,
	#credentials{
		user_id = UserID
	} = Credentials,

	NumberOfDests = length(Destinations),
	NumberOfMsgs = NumberOfDests * NumberOfParts,

	{ok, SessionID} = k1api_billy_session:get_session_id(),
	case billy_client:reserve(
		SessionID, ?CLIENT_TYPE_ONEAPI, CustomerID, UserID, ?SERVICE_TYPE_SMS_ON, NumberOfMsgs
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
					rolledback = billy_client:rollback(TransID),
					?log_debug("Rolledback.", []),
					{error, Reason}
			end;
		{rejected, Reason} ->
			?log_debug("Reserve rejected with: ~p", [Reason]),
			{error, Reason}
	end.

just_send(OutboundSms, Customer, Credentials, Encoding, NumberOfParts, Destinations) ->
	#outbound_sms{
		sender_addr = RawSenderAddress,
		message = Message,
		notify_url = NotifyURL,
		correlator = Correlator,
		callback = CallbackData
	} = OutboundSms,
	#k1api_auth_response_dto{
		customer_uuid = CustomerID,
		allowed_sources = AllowedSources,
		default_validity = DefaultValidity,
		no_retry = NoRetry
	} = Customer,
	#credentials{
		user_id = UserID
	} = Credentials,
	ReqID = uuid:unparse(uuid:generate()),
	Params = lists:flatten([
			?just_sms_request_param(<<"k1api_notify_url">>, NotifyURL),
			?just_sms_request_param(<<"k1api_callback_data">>, CallbackData),
			?just_sms_request_param(<<"registered_delivery">>, true),
			?just_sms_request_param(<<"service_type">>, <<>>),
			?just_sms_request_param(<<"no_retry">>, NoRetry),
			?just_sms_request_param(<<"validity_period">>, fmt_validity(DefaultValidity)),
			?just_sms_request_param(<<"priority_flag">>, 0),
			?just_sms_request_param(<<"esm_class">>, 3),
			?just_sms_request_param(<<"protocol_id">>, 0)
			]),
	NumberOfDests = length(Destinations),
	GtwID = get_suitable_gtw(Customer, NumberOfDests),
	MessageIDs = get_ids(CustomerID, NumberOfDests, NumberOfParts),
	?log_debug("Message IDs: ~p", [MessageIDs]),
	DTO = #just_sms_request_dto{
		id = ReqID,
		gateway_id = GtwID,
		customer_id = CustomerID,
		user_id = UserID,
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
	case k1api_db:check_correlator(CustomerID, UserID, Correlator, ReqID) of
		ok ->
			{ok, Bin} = adto:encode(DTO),
			?log_debug("SmsRequest was sucessfully encoded", []),
			ok = publish_sms_request(Bin, ReqID, GtwID),
			{ok, ReqID};
		{correlator_exist, OrigReqID} ->
			{ok, OrigReqID}
	end.

publish_sms_request(Payload, ReqID, GtwID) ->
    Basic = #'P_basic'{
        content_type = <<"k1apiSmsRequest">>,
        delivery_mode = 2,
        priority = 1,
        message_id = ReqID
    },
	{ok, Channel} = gen_server:call(?MODULE, get_channel),
	GtwQueue = binary:replace(<<"pmm.just.gateway.%id%">>, <<"%id%">>, GtwID),
	?log_debug("Sending message to ~p & ~p through the ~p", [?K1API_SMS_REQ_Q, GtwQueue, Channel]),
    ok = rmql:basic_publish(Channel, ?K1API_SMS_REQ_Q, Payload, Basic),
    ok = rmql:basic_publish(Channel, GtwQueue, Payload, Basic).


prepare_source_addr(AllowedSources, RawSenderAddress) ->
	SenderAddress = k1api_lib:addr_to_dto(RawSenderAddress),
	IsAddrAllowed = lists:filter(fun(AllowedSource) ->
		AllowedSource =:= SenderAddress
		end, AllowedSources),
	case IsAddrAllowed of
		[] ->
			erlang:error(sender_address_not_available);
		[Addr | _] ->
			Addr
	end.

addr_to_dto(OneAPIAddresses) when is_list(OneAPIAddresses) ->
	lager:debug("OneAPIAddresses: ~p", [OneAPIAddresses]),
	[k1api_lib:addr_to_dto(Addr) || Addr <- OneAPIAddresses].

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

get_message_parts(Size, default) when Size =< 160 ->
	{ok, 1};
get_message_parts(Size, default) ->
	case (Size rem 153) == 0 of
		true -> {ok, trunc(Size/153)};
		false -> {ok, trunc(Size/153) +1}
	end;
get_message_parts(Size, ucs2) when Size =< 70 ->
	{ok, 1};
get_message_parts(Size, ucs2) ->
	case (Size rem 67) == 0 of
		true -> {ok, trunc(Size/67)};
		false -> {ok, trunc(Size/67) + 1}
	end.

fmt_validity(SecondsTotal) ->
    MinutesTotal = SecondsTotal div 60,
    HoursTotal = MinutesTotal div 60,
    DaysTotal = HoursTotal div 24,
    MonthsTotal = DaysTotal div 30,
    Years = MonthsTotal div 12,
    Seconds = SecondsTotal rem 60,
    Minutes = MinutesTotal rem 60,
    Hours = HoursTotal rem 24,
    Days = DaysTotal rem 30,
    Months = MonthsTotal rem 12,
    list_to_binary(lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w000R",
                  [Years, Months, Days, Hours, Minutes, Seconds]))).
