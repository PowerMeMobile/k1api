-module(eoa_sms_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-export([behaviour_info/1]).

-compile([{parse_transform, lager_transform}]).

-include("logging.hrl").

-include("eoneapi.hrl").

% -define(gmv(ReqPropList, Key),
% 	lists:flatten(
% 		lists:map(fun({EKey, EValue})->
% 			case EKey of
% 				Key -> EValue;
% 				_ -> []
% 			end
% 		end, ReqPropList)
% 		)
% 	).

% -define(gv(ReqPropList, Key),
% 	case lists:keytake(Key, 1, ReqPropList) of
% 		{value, {_, Value}, _TupleList2} -> Value;
% 		_ -> undefined
% 	end
% 	).


-record(state, {
	req :: term(),
	mod :: atom(),
	mstate :: term(),
	creds :: credentials(),
	protocol :: binary(),
	sender_addr :: binary()
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
	% ?log_debug("Req: ~p", [Req]),
	% case credentials_check(Req) of 
	% 	{ok, {SysId, User, Pass}} ->
	% 		Creds = #credentials{system_id = SysId, user = User, password = Pass},
	% 		{Path, Req} = cowboy_http_req:path(Req),
	% 		{Method, Req} = cowboy_http_req:method(Req),
	% 		% handle_req(Method, Path, State#state{req = Req, creds = Creds});
	% 		check_req_type(Method, Path, #state{mod = Module, req = Req, creds = Creds});
	% 	{error, Error} ->
	% 		?log_debug("Error: ~p", [Error]),
	% 		eoneapi:code(401, Req, State)		
	% end.
	% auth(Creds, Module),
	?log_debug("Req: ~p", [Req]),
	{ok, Req, #state{mod = Module}}.

% check_req_type(
% 		'POST',
% 		[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"requests">>],
% 		State = #state{creds = Creds}) ->
% 	case parse_sender_addr(SenderAddr) of
% 		{ok, {Protocol, Addr}} ->
% 			process_outbound_sms_req(State#state{protocol = Protocol, sender_addr = SenderAddr});
% 		Error ->
% 			?log_error("Unexpected error: ~p", [Error]),
% 			eoneapi:code(500, Req, State)
% 	end;

handle(Req, State = #state{mod = Mod}) ->
	case credentials_check(Req) of
		{ok, {SysId, User, Pass}} ->
			?log_debug("SysId: ~p, User: ~p Pass: ~p", [SysId, User, Pass]),
			Creds = #credentials{system_id = SysId, user = User, password = Pass},
			% {Path, Req} = cowboy_http_req:path(Req),
			% {Method, Req} = cowboy_http_req:method(Req),
			% handle_req(Method, Path, State#state{req = Req, creds = Creds});
			do_init(State#state{req = Req, creds = Creds});
		{error, 'Unauthorized'} ->
			?log_debug("Error: ~p", ['Unauthorized']),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

terminate(_Req, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%

do_init(State = #state{mod = Mod, req = Req, creds = Creds}) ->
	InitResult = Mod:init(Creds),
	case InitResult of
		{ok, MState} ->	
			{Path, Req} = cowboy_http_req:path(Req),
			{Method, Req} = cowboy_http_req:method(Req),
			handle_req(Method, Path, State#state{mstate = MState});
		{error, dinied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

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

	%% Parsing http requests

handle_req('POST',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"requests">>],
			State = #state{req = Req}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			process_outbound_sms_req({Protocol, Addr}, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req('GET',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"requests">>, ReqId, <<"deliveryInfos">>],
			State = #state{req = Req}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			process_delivery_status_req({Protocol, Addr}, ReqId, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req('POST',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"subscriptions">>],
			State = #state{req = Req}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			process_sms_delivery_report_subscribe_req({Protocol, Addr}, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req('DELETE',
			[_Ver,<<"smsmessaging">>,<<"outbound">>, SenderAddr,<<"subscriptions">>, SubId],
			State = #state{req = Req}) ->
	case parse_sender_addr(SenderAddr) of
		{ok, {Protocol, Addr}} ->
			process_sms_delivery_report_unsubscribe_req({Protocol, Addr}, SubId, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end;

handle_req('GET',
			[_Ver,<<"smsmessaging">>,<<"inbound">>, <<"registrations">>, RegId,<<"messages">>],
			State = #state{req = Req}) ->
	process_retrieve_sms_req(RegId, State);

handle_req('POST',
			[_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>],
			State = #state{req = Req}) ->
	process_sms_delivery_subscribe_req(State);

handle_req('DELETE',
			[_Ver,<<"smsmessaging">>,<<"inbound">>,<<"subscriptions">>, SubId],
			State = #state{req = Req}) ->
	process_sms_delivery_unsubscribe_req(SubId, State);

handle_req(_Method, _Path, State = #state{req = Req}) ->
	eoneapi:code(404, Req, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Outbound sms request processor

process_outbound_sms_req({Protocol, ID}, State = #state{
										mstate = MState,
										mod = Mod,
										creds = Creds,
										req = Req}) ->
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
			Body = << <<"{\"resourceReference\":{\"resourceURL\":\"">>/binary, Location/binary, <<"\"}}">>/binary>>,
			{ok, Req2} = cowboy_http_req:reply(201, [{'Content-Type', ContentType}, {'Location', Location}], Body, Req),
			{ok, Req2, State};
		{error, denied} ->
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

	% delivery status request processor

process_delivery_status_req(SenderAddr, ReqId,
									State = #state{
												mod = Mod,
												mstate = MState,
												creds = Creds,
												req = Req}) ->
	Response = Mod:handle_delivery_status_req(Creds, SenderAddr, ReqId, MState),
	case Response of
		{ok, ResponseList} ->
			Reports = bjoin_comma_delimited(
			lists:map(fun({Address, DeliveryStatus})->
				AddressBin = list_to_binary(Address),
				DeliveryStatusBin = list_to_binary(DeliveryStatus),
				<<
					<<"{\"address\":\"tel:+">>/binary,
					AddressBin/binary,
					<<"\",\"deliveryStatus\":\"">>/binary,
					DeliveryStatusBin/binary,
					<<"\"}">>/binary
				>>
			end, ResponseList)),

			Resource = build_resource(Req),

			Body = << <<"{\"deliveryInfoList\":{\"deliveryInfo\":[">>/binary, Reports/binary,<<"],\"resourceURL\":\"">>/binary, Resource/binary,<<"\"}}">>/binary >>,

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

process_sms_delivery_report_subscribe_req({Protocol, Addr}, State = #state{
																req = Req,
																mod = Mod,
																mstate = MState,
																creds = Creds	
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
			Body = << <<"{\"deliveryReceiptSubscription\":{\"callbackReference\":{\"callbackData\":\"">>/binary, CallBackData/binary, <<"\",\"notifyURL\":\"">>/binary, NotifyURL/binary, <<"\",\"criteria\":\"Urgent\"},\"resourceURL\":\"">>/binary, Location/binary, <<"\"}}">>/binary >>,
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

process_sms_delivery_report_unsubscribe_req({Protocol, Addr}, SubscribeId, State = #state{
																req = Req,
																mod = Mod,
																mstate = MState,
																creds = Creds	
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
					<<
					<<"{\"dateTime\":\"">>/binary,
					DateTimeBin/binary,
					<<"\",\"destinationAddress\":\"">>/binary,
					RegId/binary,
					<<"\",\"messageId\":\"">>/binary,
					MessIdBin/binary,
					<<"\",\"message\":\"">>/binary,
					MessageTextBin/binary,
					<<"\",\"resourceURL\":\"">>/binary,
					LocationUrl/binary,
					<<"\",\"senderAddress\":\"">>/binary,
					SenderAddrBin/binary,
					<<"\"}">>/binary
					>>
				end, ListOfInboundSms)
				),
			?log_debug("tag", []),
			ThisBatchSize = list_to_binary(integer_to_list(length(ListOfInboundSms))),
			?log_debug("tag", []),

			ResourceURL = build_resource(Req),
			?log_debug("tag", []),

			PendingSmsBin = list_to_binary(integer_to_list(PendingSms)),
			?log_debug("tag", []),

			Body = <<
				<<"{\"inboundSMSMessageList\":{\"inboundSMSMessage\":[">>/binary,
				Messages/binary,
      			<<"],\"numberOfMessagesInThisBatch\":\"">>/binary,
    			ThisBatchSize/binary,
    			<<"\",\"resourceURL\":\"">>/binary,
    			ResourceURL/binary,
    			<<"\",\"totalNumberOfPendingMessages\":\"">>/binary,
    			PendingSmsBin/binary,
    			<<"\"}}">>/binary
    		>>,
			?log_debug("tag", []),

			ContentType = <<"application/json">>,
			?log_debug("tag", []),
			
			{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', ContentType}], Body, Req),
			?log_debug("tag", []),
			
			{ok, Req2, State};
		{error, denied} ->
			?log_debug("Authentication failured", []),
			eoneapi:code(401, Req, State);
		Error ->
			?log_error("Unexpected error: ~p", [Error]),
			eoneapi:code(500, Req, State)
	end.

	%% Subscribe to notifications of messages sent to your application

process_sms_delivery_subscribe_req(State = #state{
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
			Body = 
				<<
				<<"{\"resourceReference\":{\"resourceURL\":\"">>/binary,
				Location/binary,
				<<"\"}}">>/binary
				>>,
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
	[ProtocolBin, AddrBin] = binary:split(SenderAddr, [<<":+">>, <<":">>]),
	{ok, {binary_to_list(ProtocolBin), binary_to_list(AddrBin)}}.

build_location(Req, ItemId) ->
	{RawHost, _} = cowboy_http_req:raw_host(Req),
	{RawPath, _} = cowboy_http_req:raw_path(Req),
	Protocol = <<"http://">>,
	ReqIdBin = list_to_binary("/" ++ ItemId),
	Location = <<Protocol/binary, RawHost/binary, RawPath/binary, ReqIdBin/binary>>,
	?log_debug("Location: ~p", [Location]),
	Location.

gv(ReqPropList, Key) ->
	case lists:keytake(Key, 1, ReqPropList) of
		{value, {_, Value}, _TupleList2} -> Value;
		_ -> undefined
	end.

gmv(ReqPropList, Key) ->
	lists:flatten(
		lists:map(fun({EKey, EValue})->
			case EKey of
				Key -> EValue;
				_ -> []
			end
		end, ReqPropList)
		).

build_resource(Req) ->
	{RawHost, _} = cowboy_http_req:raw_host(Req),
	{RawPath, _} = cowboy_http_req:raw_path(Req),
	Protocol = <<"http://">>,
	Resource = <<Protocol/binary, RawHost/binary, RawPath/binary>>,
	?log_debug("Resource: ~p", [Resource]),
	Resource.

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