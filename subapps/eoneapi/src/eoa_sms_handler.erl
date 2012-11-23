-module(eoa_sms_handler).

-behaviour(cowboy_http_handler).

-export([
	init/3,
	handle/2,
	terminate/2
]).

-include("logging.hrl").
-include("eoneapi.hrl").

-record(state, {
	req 		:: term(),
	mod 		:: atom(),
	mstate 		:: term(),
	creds 		:: credentials(),
	sender_addr :: binary(),
	thendo 		:: fun(),
	thendo_args :: term()
}).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================

-callback init(credentials()) ->
	{ok, state()} |
	{error, denied}.

-callback handle_send_sms_req(outbound_sms(), state()) ->
	{ok, request_id()} |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

-callback handle_delivery_status_req(sender_address(), request_id(), state()) ->
	{ok, sms_delivery_statuses()}  |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

-callback handle_delivery_notifications_subscribe(delivery_receipt_subscribe(), state()) ->
	{ok, subscription_id()} |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

-callback handle_delivery_notifications_unsubscribe(sender_address(), subscription_id(), state()) ->
	{ok, deleted} |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

-callback handle_retrieve_req(retrieve_sms_req(), state()) ->
	{ok, [inbound_sms()], pending_sms()} |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

-callback handle_inbound_subscribe(subscribe_inbound(), state()) ->
	{ok, subscription_id()} |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

-callback handle_inbound_unsubscribe(subscription_id(), state()) ->
	{ok, deleted} |
	{exception, exception()} |
	{exception, exception(), excep_params()}.

%% ===================================================================
%% API
%% ===================================================================

init({_Any, http}, Req, [Module]) ->
	?log_debug("Req: ~p", [Req]),
	{ok, Req, #state{mod = Module, req = Req}}.

handle(Req, State = #state{}) ->
	{Path, Req} = cowboy_http_req:path(Req),
	{Method, Req} = cowboy_http_req:method(Req),
	case get_credentials(Req) of
		{ok, {SysId, User, Pass}} ->
			Creds = #credentials{system_id = SysId, user_id = User, password = Pass},
			handle_req(Method, Path, State#state{creds = Creds});
		{error, unauthorized} ->
			?log_debug("Unauthorized", []),
			eoneapi:code(401, Req, [])
	end.

terminate(_Req, _State) ->
	ok.

%% ===================================================================
%% Parsing http requests
%% ===================================================================

handle_req(	'POST',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, RawSenderAddr,<<"requests">>],
			State = #state{req = Req, creds = Creds}) ->
	SenderAddr = convert_addr(RawSenderAddr),
	AfterInit = fun(Args, St) -> process_outbound_sms_req(Args,St) end,
	Args = [],
	do_init(State#state{
					sender_addr = SenderAddr,
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds });

handle_req(	'GET',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, RawSenderAddr,<<"requests">>, ReqId, <<"deliveryInfos">>],
			State = #state{req = Req, creds = Creds}) ->
	SenderAddr = convert_addr(RawSenderAddr),
	AfterInit = fun(Args, St) -> process_delivery_status_req(Args,St) end,
	Args = ReqId,
	do_init(State#state{
					sender_addr = SenderAddr,
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds	});

handle_req(	'POST',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, RawSenderAddr,<<"subscriptions">>],
						State = #state{req = Req, creds = Creds}) ->
	SenderAddr = convert_addr(RawSenderAddr),
	?log_debug("SenderAddr: ~p", [SenderAddr]),
	AfterInit = fun(Args, St) -> process_sms_delivery_report_subscribe_req(Args,St) end,
	Args = [],
	do_init(State#state{
					sender_addr = SenderAddr,
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds	});

handle_req(	'DELETE',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, RawSenderAddr,<<"subscriptions">>, SubId],
						State = #state{req = Req, creds = Creds}) ->
	SenderAddr = convert_addr(RawSenderAddr),
	AfterInit = fun(Args, St) -> process_sms_delivery_report_unsubscribe_req(Args,St) end,
	Args = SubId,
	do_init(State#state{
					sender_addr = SenderAddr,
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds	});

handle_req(	'GET',
			[_Ver,<<"smsmessaging">>,<<"inbound">>, <<"registrations">>, RegId,<<"messages">>],
			State = #state{req = Req, creds = Creds}) ->
	AfterInit = fun(Args, St) -> process_retrieve_sms_req(Args,St) end,
	Args = convert_addr(RegId),
	do_init(State#state{
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds	});

handle_req('POST',
			[_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>],
			State = #state{req = Req, creds = Creds}) ->
	AfterInit = fun(Args, St) -> process_sms_delivery_subscribe_req(Args,St) end,
	Args = [],
	do_init(State#state{
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds	});

handle_req('DELETE',
			[_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>, SubId],
			State = #state{req = Req, creds = Creds}) ->
	AfterInit = fun(Args, St) -> process_sms_delivery_unsubscribe_req(Args,St) end,
	Args = SubId,
	do_init(State#state{
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds	});

handle_req(_Method, _Path, State = #state{req = Req}) ->
	eoneapi:code(404, Req, State).

%% ===================================================================
%% Handler initialization
%% ===================================================================

do_init(State = #state{
					thendo = Fun,
					thendo_args = Args,
					mod = Mod,
					req = Req,
					creds = Creds}) ->
	InitResult = Mod:init(Creds),
	case InitResult of
		{ok, MState} ->
			Fun(Args, State#state{mstate = MState});
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% 	Outbound sms request
%% ===================================================================

process_outbound_sms_req( _, State = #state{
										mstate = MState,
										mod = Mod,
										req = Req,
										sender_addr = _Addr}) ->
	{ok, ReqPropList} = get_prop_list(Req),
	SendSmsReq = #outbound_sms{
					dest_addr = gmv(ReqPropList, <<"address">>),
					sender_addr = gv(ReqPropList, <<"senderAddress">>),
					message = gv(ReqPropList, <<"message">>),
					sender_name = gv(ReqPropList, <<"senderName">>), %opt
					notify_url = gv(ReqPropList, <<"notifyURL">>), %% opt
					correlator = gv(ReqPropList, <<"clientCorrelator">>), %opt
					callback = gv(ReqPropList, <<"callbackData">>) % opt
					},
	Result =
		Mod:handle_send_sms_req(SendSmsReq, MState),
	case Result of
		{ok, ReqId} ->
			ContentType = <<"application/json">>,
			Location = build_location(Req, ReqId),
			Body =
			[{<<"resourceReference">>, [
				{<<"resourceURL">>, Location}
			]}],
			JsonBody = jsx:encode(Body),
			Headers = [{'Content-Type', ContentType}, {'Location', Location}],
			{ok, Req2} = cowboy_http_req:reply(201, Headers, JsonBody, Req),
			{ok, Req2, State};
		{error, denied} ->
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Delivery Status Request
%% ===================================================================

process_delivery_status_req(ReqId,
									State = #state{
												mod = Mod,
												mstate = MState,
												%creds = Creds,
												req = Req,
												sender_addr = SAddr}) ->
	Response = Mod:handle_delivery_status_req(SAddr, ReqId, MState),
	case Response of
		{ok, ResponseList} ->
			Reports =
				lists:map(fun({Address, DeliveryStatus})->
				[{<<"address">>, Address}, {<<"deliveryStatus">>, DeliveryStatus}]
				end, ResponseList),
			Resource = build_resource(Req),
			Body =
			[{<<"deliveryInfoList">>, [
				{<<"deliveryInfo">>, Reports},
				{<<"resourceURL">>, Resource}
			]}],
			JsonBody = jsx:encode(Body),
			Headers = [{'Content-Type', <<"application/json">>}],
			{ok, Req2} = cowboy_http_req:reply(200, Headers, JsonBody, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Start subscribe to SMS delivery notifications
%% ===================================================================

process_sms_delivery_report_subscribe_req(_, State = #state{
																req = Req,
																mod = Mod,
																mstate = MState,
																%creds = Creds,
																sender_addr = Addr
															}) ->
	{ok, ReqPropList} = get_prop_list(Req),
	Request = #delivery_receipt_subscribe{
					sender_addr = Addr,
					notify_url = gv(ReqPropList, <<"notifyURL">>),
					correlator = gv(ReqPropList, <<"clientCorrelator">>), % opt
					criteria = gv(ReqPropList, <<"criteria">>), % opt
					callback = gv(ReqPropList, <<"callbackData">>) % opt
					},
	Result = Mod:handle_delivery_notifications_subscribe(Request, MState),
	case Result of
		{ok, SubscribeId} ->
			CallBackData = gv(ReqPropList, <<"callbackData">>),
			NotifyURL = gv(ReqPropList, <<"notifyURL">>),
			Location = build_location(Req, SubscribeId),
			ContentType = <<"application/json">>,
			Criteria = gv(ReqPropList, <<"criteria">>),
			Body =
			[{<<"deliveryReceiptSubscription">>, [
				{<<"callbackReference">>, [
					{<<"callbackData">>, CallBackData},
					{<<"notifyURL">>, NotifyURL},
					{<<"criteria">>, Criteria}
				]},
				{<<"resourceURL">>, Location}
			]}],
			JsonBody = jsx:encode(Body),
			Headers = [{'Content-Type', ContentType}, {'Location', Location}],
			{ok, Req2} = cowboy_http_req:reply(201, Headers, JsonBody, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Stop the subscription to delivery notifications
%% ===================================================================

process_sms_delivery_report_unsubscribe_req(SubscribeId, State = #state{
																req = Req,
																mod = Mod,
																mstate = MState,
																creds = Creds,
																sender_addr = Addr
															}) ->
	Result = Mod:handle_delivery_notifications_unsubscribe(Addr, SubscribeId, MState),
	case Result of
		{ok, deleted} ->
			{ok, Req2} = cowboy_http_req:reply(204, [], <<>>, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Retrieve messages sent to your Web application
%% ===================================================================

process_retrieve_sms_req(RegId, State = #state{
											mod = Mod,
											mstate = MState,
											%creds = Creds,
											req = Req
											}) ->
	{ok, ReqPropList} = get_prop_list(Req),
	RetrieveSmsReq = #retrieve_sms_req{
						reg_id = RegId,
						batch_size = giv(ReqPropList, <<"maxBatchSize">>)
						},
	Result = Mod:handle_retrieve_req(RetrieveSmsReq, MState),
	case Result of
		{ok, ListOfInboundSms, PendingSms} ->
			Messages =
				lists:map(fun(#inbound_sms{
								date_time = DateTime,
								message_id = MessIdBin,
								message = MessageTextBin,
								sender_addr = SenderAddrBin})->
					DateTimeBin = iso8601:format(DateTime),
					LocationUrl = build_location(Req, MessIdBin),
					[{<<"dateTime">>, DateTimeBin},
					{<<"destinationAddress">>, RegId},
					{<<"messageId">>, MessIdBin},
					{<<"message">>, MessageTextBin},
					{<<"resourceURL">>, LocationUrl},
					{<<"senderAddress">>, SenderAddrBin}]
				end, ListOfInboundSms),
			ThisBatchSize = length(ListOfInboundSms),
			ResourceURL = build_resource(Req),
			Body =
			[{<<"inboundSMSMessageList">>, [
				{<<"inboundSMSMessage">>, Messages},
				{<<"numberOfMessagesInThisBatch">>, ThisBatchSize},
				{<<"resourceURL">>, ResourceURL},
				{<<"totalNumberOfPendingMessages">>, PendingSms}
			]}],
			JsonBody = jsx:encode(Body),
			Headers = [{'Content-Type', <<"application/json">>}],
			{ok, Req2} = cowboy_http_req:reply(200, Headers, JsonBody, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Subscribe to notifications of messages sent to your application
%% ===================================================================

process_sms_delivery_subscribe_req( _, State = #state{
												req = Req,
												mod = Mod,
												mstate = MState%,
												%creds = Creds
												}) ->

	{ok, ReqPropList} = get_prop_list(Req),
	SubscribeInbound = #subscribe_inbound{
							dest_addr = convert_addr(gv(ReqPropList, <<"destinationAddress">>)),
							notify_url = gv(ReqPropList, <<"notifyURL">>),
							criteria = gv(ReqPropList, <<"criteria">>), % opt
							callback = gv(ReqPropList, <<"callbackData">>), % opt
							correlator = gv(ReqPropList, <<"clientCorrelator">>) % opt
						},
	case Mod:handle_inbound_subscribe(SubscribeInbound, MState) of
		{ok, SubId} ->
			Location = build_location(Req, SubId),
			ContentType = <<"application/json">>,
			Body =
			[{<<"resourceReference">>, [
				{<<"resourceURL">>, Location}
			]}],
			JsonBody = jsx:encode(Body),
			Headers = [{'Location', Location}, {'Content-Type', ContentType}],
			{ok, Req2} = cowboy_http_req:reply(201, Headers, JsonBody, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Stop the subscription to message notifications
%% ===================================================================

process_sms_delivery_unsubscribe_req(SubId, State = #state{
														mod = Mod,
														mstate = MState,
														req = Req%,
														%creds = Creds
														}) ->
	case Mod:handle_inbound_unsubscribe(SubId, MState) of
		{ok, deleted} ->
			{ok, Req2} = cowboy_http_req:reply(204, [], <<>>, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

%% ===================================================================
%% Local
%% ===================================================================

get_prop_list(Req) ->
	{Method, _} = cowboy_http_req:method(Req),
	ReqPropList =
	case Method of
		'POST' ->
			{BodyQs, _} = cowboy_http_req:body_qs(Req),
			{QsVals, _} = cowboy_http_req:qs_vals(Req),
			BodyQs ++ QsVals;
		_Any ->
			{QsVals, _} = cowboy_http_req:qs_vals(Req),
			QsVals
	end,
	?log_debug("ReqPropList: ~p", [ReqPropList]),
	{ok, ReqPropList}.

convert_addr(<<"tel:+", Bin/binary>>) ->
	Bin;
convert_addr(Bin) when is_binary(Bin) ->
	Bin.

giv(ReqPropList, Key) ->
	case gv(ReqPropList, Key) of
		undefined -> undefined;
		Value -> list_to_integer(binary_to_list(Value))
	end.

gv(ReqPropList, Key) ->
	case lists:keytake(Key, 1, ReqPropList) of
		{value, {_, Value}, _TupleList2} -> Value;
		_ -> undefined
	end.

gmv(ReqPropList, Key) ->
	lists:flatten(
		lists:map(fun({K, V})->
			case K of
				Key -> V;
				_ -> []
			end
		end, ReqPropList)
		).

build_location(Req, ItemId) when is_binary(ItemId) ->
	{RawHost, _} = cowboy_http_req:raw_host(Req),
	{RawPath, _} = cowboy_http_req:raw_path(Req),
	Protocol = <<"http://">>,
	ReqIdBin = << <<"/">>/binary, ItemId/binary>>,
	<<Protocol/binary, RawHost/binary, RawPath/binary, ReqIdBin/binary>>.

build_resource(Req) ->
	{RawHost, _} = cowboy_http_req:raw_host(Req),
	{RawPath, _} = cowboy_http_req:raw_path(Req),
	Protocol = <<"http://">>,
	<<Protocol/binary, RawHost/binary, RawPath/binary>>.

%% ===================================================================
%% Credentials
%% ===================================================================

get_credentials(Req) ->
	{Header, Req} = cowboy_http_req:header('Authorization', Req),
	case application:get_env(eoneapi, sysid_user_delimiter) of
		{ok, Delimiter} ->
			parse_credential_header(Header, [Delimiter]);
		undefined ->
			parse_credential_header(Header, [])
	end.

parse_credential_header(undefined, _Delimiter) ->
	{error, unauthorized};
parse_credential_header(Header, Delimiter) ->
	RawList = binary:split(Header, [<<"Basic">>, <<" ">>],[global]),
	[Base64Bin] = lists:filter(fun(Elem) -> Elem =/= <<>> end, RawList),
	CredsBin = base64:decode(Base64Bin),
	[SysIdBin, UserBin, PassBin] = binary:split(CredsBin, [<<":">>] ++ Delimiter,[global]),
	{ok, {SysIdBin, UserBin, PassBin}}.
