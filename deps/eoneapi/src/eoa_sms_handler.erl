-module(eoa_sms_handler).
%% TO DO 
%% * intelligent binary join that able to exclude undefined paramaters
%% * implement exceptions (http://oneapi.gsmworld.com/common-policy-exceptions/)
-behaviour(cowboy_http_handler).

-export([
	init/3, 
	handle/2,
	terminate/2
]).

-export([behaviour_info/1]).

-compile([{parse_transform, lager_transform}]).

-include("logging.hrl").
-include("eoneapi.hrl").

-record(state, {
	req :: term(),
	mod :: atom(),
	mstate :: term(),
	creds :: credentials(),
	protocol :: binary(),
	sender_addr :: binary(),
	thendo :: fun(),
	thendo_args :: term()
	}).

%%%%%%%%%%%%%%%%
%% API functions
%%%%%%%%%%%%%%%%

behaviour_info(callbacks) ->
	[
		{init, 1},
		{handle_send_sms_req, 3},
		{handle_delivery_status_req,4},
		{handle_delivery_notifications_subscribe,3},
		{handle_delivery_notifications_unsubscribe,4},
		{handle_retrieve_req,3},
		{handle_inbound_subscribe,3},
		{handle_inbound_unsubscribe,3}
	].

init({_Any, http}, Req, [Module]) ->
	?log_debug("Req: ~p", [Req]),
	{ok, Req, #state{mod = Module, req = Req}}.

handle(Req, State = #state{}) ->
	{Path, Req} = cowboy_http_req:path(Req),
	{Method, Req} = cowboy_http_req:method(Req),
	case credentials_check(Req) of 
		{ok, {SysId, User, Pass}} ->
			Creds = #credentials{system_id = SysId, user = User, password = Pass},
			handle_req(Method, Path, State#state{creds = Creds});
		{error, Error} ->
			?log_debug("Error: ~p", [Error]),
			eoneapi:code(401, Req, []);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, [])	
	end.

terminate(_Req, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%

	%% Parsing http requests

handle_req(	'POST',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"requests">>],
			State = #state{req = Req, creds = Creds}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			AfterInit = fun(Args, St) -> process_outbound_sms_req(Args,St) end,
			Args = [],
			do_init(State#state{
							protocol = Protocol,
							sender_addr = Addr,
							thendo = AfterInit,
							thendo_args = Args,
							creds = Creds#credentials{type = transmitter}});
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req(	'GET',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"requests">>, ReqId, <<"deliveryInfos">>],
			State = #state{req = Req, creds = Creds}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			AfterInit = fun(Args, St) -> process_delivery_status_req(Args,St) end,
			Args = binary_to_list(ReqId),
			do_init(State#state{
							protocol = Protocol,
							sender_addr = Addr,
							thendo = AfterInit,
							thendo_args = Args,
							creds = Creds#credentials{type = dlvrReceiptReceiver}});
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req(	'POST',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"subscriptions">>],
						State = #state{req = Req, creds = Creds}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			AfterInit = fun(Args, St) -> process_sms_delivery_report_subscribe_req(Args,St) end,
			Args = [],
			do_init(State#state{
							protocol = Protocol,
							sender_addr = Addr,
							thendo = AfterInit,
							thendo_args = Args,
							creds = Creds#credentials{type = dlvrReceiptReceiver}});
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req(	'DELETE',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"subscriptions">>, SubId],
						State = #state{req = Req, creds = Creds}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			AfterInit = fun(Args, St) -> process_sms_delivery_report_unsubscribe_req(Args,St) end,
			Args = binary_to_list(SubId),
			do_init(State#state{
							protocol = Protocol,
							sender_addr = Addr,
							thendo = AfterInit,
							thendo_args = Args,
							creds = Creds#credentials{type = dlvrReceiptReceiver}});
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req(	'GET',
			[_Ver,<<"smsmessaging">>,<<"inbound">>, <<"registrations">>, RegId,<<"messages">>],
			State = #state{req = Req, creds = Creds}) ->
	AfterInit = fun(Args, St) -> process_retrieve_sms_req(Args,St) end,
	Args = RegId,
	do_init(State#state{
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds#credentials{type = incomingSMSReceiver}});

handle_req('POST',
			[_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>],
			State = #state{req = Req, creds = Creds}) ->
	AfterInit = fun(Args, St) -> process_sms_delivery_subscribe_req(Args,St) end,
	Args = [],
	do_init(State#state{
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds#credentials{type = incomingSMSReceiver}});

handle_req('DELETE',
			[_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>, SubId],
			State = #state{req = Req, creds = Creds}) ->
	AfterInit = fun(Args, St) -> process_sms_delivery_unsubscribe_req(Args,St) end,
	Args = SubId,
	do_init(State#state{
					thendo = AfterInit,
					thendo_args = Args,
					creds = Creds#credentials{type = incomingSMSReceiver}});

handle_req(_Method, _Path, State = #state{req = Req}) ->
	eoneapi:code(404, Req, State).

  %% Handler initialization

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Outbound sms request processor

process_outbound_sms_req( _ , State = #state{
										mstate = MState,
										mod = Mod,
										creds = Creds,
										req = Req,
										protocol = Protocol,
										sender_addr = Addr}) ->		
	{ok, ReqPropList} = get_prop_list(Req),
	SendSmsReq = #outbound_sms{
					address = gmv(ReqPropList, <<"address">>),
					sender_address = gv(ReqPropList, <<"senderAddress">>),
					message = gv(ReqPropList, <<"message">>),
					sender_name = gv(ReqPropList, <<"senderName">>), %opt
					notify_url = gv(ReqPropList, <<"notifyURL">>), %% opt
					client_correlator = gv(ReqPropList, <<"clientCorrelator">>), %opt
					callback_data = gv(ReqPropList, <<"callbackData">>) % opt
					},
	Result = 
		Mod:handle_send_sms_req(Creds, SendSmsReq, MState),
	case Result of
		{ok, ReqId} ->
			ContentType = <<"application/json">>,
			Location = build_location(Req, ReqId),
			Body = bjoin([<<"{\"resourceReference\":{\"resourceURL\":\"">>, Location, <<"\"}}">>]),
			{ok, Req2} = cowboy_http_req:reply(201, [{'Content-Type', ContentType}, {'Location', Location}], Body, Req),
			{ok, Req2, State};
		{error, denied} ->
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

	% delivery status request processor
	
process_delivery_status_req(ReqId,
									State = #state{
												mod = Mod,
												mstate = MState,
												creds = Creds,
												req = Req,
												sender_addr = SAddr}) ->
	Response = Mod:handle_delivery_status_req(Creds, SAddr, ReqId, MState),
	case Response of
		{ok, ResponseList} ->
			Reports = bjoin_comma_delimited(
			lists:map(fun({Address, DeliveryStatus})->
				AddressBin = list_to_binary(Address),
				DeliveryStatusBin = list_to_binary(DeliveryStatus),
				bjoin([<<"{\"address\":\"tel:+">>, AddressBin, <<"\",\"deliveryStatus\":\"">>, DeliveryStatusBin, <<"\"}">>])
			end, ResponseList)),

			Resource = build_resource(Req),
			Body = bjoin([<<"{\"deliveryInfoList\":{\"deliveryInfo\":[">>, Reports, <<"],\"resourceURL\":\"">>, Resource, <<"\"}}">>]),

			ContentType = <<"application/json">>,

			{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', ContentType}], Body, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.


	%% Start subscribe to SMS delivery notifications

process_sms_delivery_report_subscribe_req(_, State = #state{
																req = Req,
																mod = Mod,
																mstate = MState,
																creds = Creds,
																protocol = Protocol,
																sender_addr = Addr	
															}) ->
	{ok, ReqPropList} = get_prop_list(Req),
	Request = #del_rec_subscribe{
					sender_address = {Protocol, Addr},
					notify_url = gv(ReqPropList, <<"notifyURL">>),
					client_correlator = gv(ReqPropList, <<"clientCorrelator">>), % opt
					criteria = gv(ReqPropList, <<"criteria">>), % opt
					callback_data = gv(ReqPropList, <<"callbackData">>) % opt
					},
	Result = Mod:handle_delivery_notifications_subscribe(Creds, Request, MState),
	case Result of
		{ok, SubscribeId} ->
			CallBackData = gv(ReqPropList, <<"callbackData">>),
			NotifyURL = gv(ReqPropList, <<"notifyURL">>),
			Location = build_location(Req, SubscribeId),
			ContentType = <<"application/json">>,
			Criteria = gv(ReqPropList, <<"criteria">>),
			Body = bjoin([<<"{\"deliveryReceiptSubscription\":{\"callbackReference\":{\"callbackData\":\"">>, CallBackData, <<"\",\"notifyURL\":\"">>, NotifyURL, <<"\",\"criteria\":\"">>, Criteria, <<"\"},\"resourceURL\":\"">>, Location, <<"\"}}">>]),
			{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', ContentType}, {'Location', Location}], Body, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

	%% Stop the subscription to delivery notifications

process_sms_delivery_report_unsubscribe_req(SubscribeId, State = #state{
																req = Req,
																mod = Mod,
																mstate = MState,
																creds = Creds,
																protocol = Protocol,
																sender_addr = Addr
															}) ->
	Result = Mod:handle_delivery_notifications_unsubscribe(Creds, {Protocol, Addr}, SubscribeId, MState),
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

	%% Retrieve messages sent to your Web application

process_retrieve_sms_req(RegId, State = #state{
											mod = Mod,
											mstate = MState,
											creds = Creds,
											req = Req
											}) ->
	{ok, ReqPropList} = get_prop_list(Req),
	RetrieveSmsReq = #retrieve_sms_req{
						registration_id = RegId,
						batch_size = gv(ReqPropList, <<"maxBatchSize">>)
						},
	Result = Mod:handle_retrieve_req(Creds, RetrieveSmsReq, MState),
	case Result of
		{ok, ListOfInboundSms, PendingSms} ->
			Messages = bjoin_comma_delimited(
				lists:map(fun(#inbound_sms{
								date_time = DateTime,
								message_id = MessId,
								message = MessageText,
								sender_address = SenderAddr})->
					DateTimeBin = iso8601:format(DateTime),
					MessIdBin = list_to_binary(MessId),
					MessageTextBin = list_to_binary(MessId),
					SenderAddrBin = list_to_binary(SenderAddr),
					LocationUrl = build_location(Req, MessId),
					bjoin([
						<<"{\"dateTime\":\"">>,
						DateTimeBin,
						<<"\",\"destinationAddress\":\"">>,
						RegId,
						<<"\",\"messageId\":\"">>,
						MessIdBin,
						<<"\",\"message\":\"">>,
						MessageTextBin,
						<<"\",\"resourceURL\":\"">>,
						LocationUrl,
						<<"\",\"senderAddress\":\"">>,
						SenderAddrBin,
						<<"\"}">>
						])
				end, ListOfInboundSms)
				),
% <<<<<<< Updated upstream
			ThisBatchSize = list_to_binary(integer_to_list(length(ListOfInboundSms))),

			ResourceURL = build_resource(Req),

			PendingSmsBin = list_to_binary(integer_to_list(PendingSms)),

			Body = bjoin([
				<<"{\"inboundSMSMessageList\":{\"inboundSMSMessage\":[">>,
				Messages,
				<<"],\"numberOfMessagesInThisBatch\":\"">>,
				ThisBatchSize,
				<<"\",\"resourceURL\":\"">>,
				ResourceURL,
				<<"\",\"totalNumberOfPendingMessages\":\"">>,
				PendingSmsBin,
				<<"\"}}">>
				]),

			ContentType = <<"application/json">>,
			
			{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', ContentType}], Body, Req),
% =======
% 			% ?log_debug("tag", []),
% 			ThisBatchSize = list_to_binary(integer_to_list(length(ListOfInboundSms))),
% 			% ?log_debug("tag", []),

% 			ResourceURL = build_resource(Req),
% 			% ?log_debug("tag", []),

% 			PendingSmsBin = list_to_binary(integer_to_list(PendingSms)),
% 			% ?log_debug("tag", []),

% 			Body = <<
% 				<<"{\"inboundSMSMessageList\":{\"inboundSMSMessage\":[">>/binary,
% 				Messages/binary,
%       			<<"],\"numberOfMessagesInThisBatch\":\"">>/binary,
%     			ThisBatchSize/binary,
%     			<<"\",\"resourceURL\":\"">>/binary,
%     			ResourceURL/binary,
%     			<<"\",\"totalNumberOfPendingMessages\":\"">>/binary,
%     			PendingSmsBin/binary,
%     			<<"\"}}">>/binary
%     		>>,
% 			% ?log_debug("tag", []),

% 			ContentType = <<"application/json">>,
% 			% ?log_debug("tag", []),
			
% 			{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', ContentType}], Body, Req),
% 			% ?log_debug("tag", []),
% >>>>>>> Stashed changes
			
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

	%% Subscribe to notifications of messages sent to your application

process_sms_delivery_subscribe_req( _, State = #state{
												req = Req,
												mod = Mod,
												mstate = MState,
												creds = Creds
												}) ->
	
	{ok, ReqPropList} = get_prop_list(Req),
	SubscribeInbound = #subscribe_inbound{
							destination_address = gv(ReqPropList, <<"destinationAddress">>),
							notify_url = gv(ReqPropList, <<"notifyURL">>),
							criteria = gv(ReqPropList, <<"criteria">>), % opt
							callback_data = gv(ReqPropList, <<"callbackData">>), % opt
							client_correlator = gv(ReqPropList, <<"clientCorrelator">>) % opt
						},
	case Mod:handle_inbound_subscribe(Creds, SubscribeInbound, MState) of
		{ok, SubId} ->
			Location = build_location(Req, SubId),
			ContentType = <<"application/json">>,
			Body = bjoin([
				<<"{\"resourceReference\":{\"resourceURL\":\"">>,
				Location,
				<<"\"}}">>
				]),
			{ok, Req2} = cowboy_http_req:reply(201, [{'Location', Location}, {'Content-Type', ContentType}], Body, Req),
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

	%% Stop the subscription to message notifications

process_sms_delivery_unsubscribe_req(SubId, State = #state{
														mod = Mod,
														mstate = MState,
														req = Req,
														creds = Creds
														}) ->
	case Mod:handle_inbound_unsubscribe(Creds, SubId, MState) of
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
%%
%% UTILS
%%

get_prop_list(Req) ->
	{Method, _} = cowboy_http_req:method(Req),
	case Method of
		'POST' ->
			{BodyQs, _} = cowboy_http_req:body_qs(Req),
			{QsVals, _} = cowboy_http_req:qs_vals(Req),
			ReqPropList = BodyQs ++ QsVals;
		Any ->
			{QsVals, _} = cowboy_http_req:qs_vals(Req),
			ReqPropList = QsVals
	end,
	?log_debug("ReqPropList: ~p", [ReqPropList]),
	{ok, ReqPropList}.

parse_sender_addr(SenderAddr) ->
	case binary:split(SenderAddr, [<<":+">>, <<":">>]) of
		[ProtocolBin, AddrBin] ->
			{ok, {binary_to_list(ProtocolBin), binary_to_list(AddrBin)}};
		[AddrBin] ->
			{ok, {"", binary_to_list(AddrBin)}}
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

build_location(Req, ItemId) ->
	{RawHost, _} = cowboy_http_req:raw_host(Req),
	{RawPath, _} = cowboy_http_req:raw_path(Req),
	Protocol = <<"http://">>,
	ReqIdBin = list_to_binary("/" ++ ItemId),
	Location = <<Protocol/binary, RawHost/binary, RawPath/binary, ReqIdBin/binary>>,
	?log_debug("Location: ~p", [Location]),
	Location.

build_resource(Req) ->
	{RawHost, _} = cowboy_http_req:raw_host(Req),
	{RawPath, _} = cowboy_http_req:raw_path(Req),
	Protocol = <<"http://">>,
	Resource = <<Protocol/binary, RawHost/binary, RawPath/binary>>,
	?log_debug("Resource: ~p", [Resource]),
	Resource.

bjoin(List) ->
    F = fun(A, B) ->
    	<<A/binary, B/binary>>
    end,
	lists:foldr(F, <<>>, List).

bjoin_comma_delimited(List) ->
    F = fun(A, B) ->
    	case B of
    		<<>> ->
    			<<A/binary, B/binary>>;
    		Any ->
    			<<A/binary, <<",">>/binary, B/binary>>
    	end
    end,
	lists:foldr(F, <<>>, List).

%%%%%%%%%%%%%%% Credentials check %%%%%%%%%%%%%%%%

credentials_check(Req) ->
	{Header, Req} = cowboy_http_req:header('Authorization', Req),
	% ?log_debug("Header: ~p", [Header]),
	case application:get_env(eoneapi, sysid_user_delimiter) of
		{ok, Delimiter} ->
			% ?log_debug("Delimiter: ~p", [Delimiter]),
			parse_credential_header(Header, [Delimiter]);
		undefined ->
			parse_credential_header(Header, [])
	end.


parse_credential_header(undefined, _Delimiter) ->
	{error, 'Unauthorized'};
parse_credential_header(Header, Delimiter) ->
	RawList = binary:split(Header, [<<"Basic">>, <<" ">>],[global]),
	[Base64Bin] = lists:filter(fun(Elem) -> Elem =/= <<>> end, RawList),
	% ?log_debug("Base64Bin: ~p", [Base64Bin]),
	UserPassBin = base64:decode(Base64Bin),
	[SysIdBin, UserBin, PassBin] = binary:split(UserPassBin, [<<":">>] ++ Delimiter,[global]),
	% ?log_debug("SysIdBin: ~p, UserBin: ~p, PassBin: ~p", [SysIdBin, UserBin, PassBin]),
	{ok, {binary_to_list(SysIdBin), binary_to_list(UserBin), binary_to_list(PassBin)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%