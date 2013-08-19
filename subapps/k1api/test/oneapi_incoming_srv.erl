-module(oneapi_incoming_srv).

-behaviour(gen_server).
-behaviour(cowboy_http_handler). %% here conflict of behaviours, but it isn't matter now

%% API
-export([
	start/0,
	give_sms/0,
	give_receipts/0
]).

%% Cowboy Callbacks
-export([
	init/3,
	handle/2
	%% terminate/2
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

-record(st, {
	sms = [] 		:: binary(),
	receipts = [] 	:: binary()
}).

-define(PORT, 44444).

%% ===================================================================
%% API
%% ===================================================================

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

give_sms() ->
	gen_server:call(?MODULE, give_sms).

give_receipts() ->
	gen_server:call(?MODULE, give_receipts).

save_sms(Sms) ->
	gen_server:cast(?MODULE, {save_sms, Sms}).

save_receipt(Receipt) ->
	gen_server:cast(?MODULE, {save_receipt, Receipt}).

%% ===================================================================
%% Cowboy Callbacks
%% ===================================================================

init({tcp, http}, Req, [Type]) ->
    {ok, Req, Type}.

handle(Req, State = receipts) ->
	{Method, _} = cowboy_http_req:method(Req),
	case Method of
		'POST' ->
			{ok, Body, _} = cowboy_http_req:body(Req),
			save_receipt(Body),
		    {ok, Req2} = cowboy_http_req:reply(201, [], <<"Created">>, Req),
		    {ok, Req2, State};
		_Any ->
		    {ok, Req2} = cowboy_http_req:reply(400, [], <<"Improper method">>, Req),
		    {ok, Req2, State}
	end;
handle(Req, State = incoming_sms) ->
	{Method, _} = cowboy_http_req:method(Req),
	case Method of
		'POST' ->
			{ok, Body, _} = cowboy_http_req:body(Req),
			save_sms(Body),
		    {ok, Req2} = cowboy_http_req:reply(201, [], <<"Created">>, Req),
		    {ok, Req2, State};
		_Any ->
		    {ok, Req2} = cowboy_http_req:reply(400, [], <<"Improper method">>, Req),
		    {ok, Req2, State}
	end.

%% terminate(Req, State) ->
%%     ok.

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
	ok = application:start(cowboy),
	Dispatch = [
	    %% {Host, list({Path, Handler, Opts})}
	   	{'_', [
			%% REST API
            {[<<"incoming_sms">>], ?MODULE, [incoming_sms]},
            {[<<"receipts">>], ?MODULE, [receipts]}
			]}],
	%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	cowboy:start_listener(my_http_listener, 1,
	    cowboy_tcp_transport, [{port, ?PORT}],
	    cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	{ok, #st{}}.

handle_call(give_sms, _From, State = #st{sms = Sms}) ->
	{reply, {ok, Sms}, State#st{sms = []}};
handle_call(give_receipts, _From, State = #st{receipts = Receipts}) ->
	{reply, {ok, Receipts}, State#st{receipts = []}};
handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast({save_sms, Sms}, State = #st{sms = SMSes}) ->
	{noreply, State#st{sms = [Sms | SMSes]}};
handle_cast({save_receipt, Receipt}, State = #st{receipts = Receipts}) ->
	{noreply, State#st{receipts = [Receipt | Receipts]}};
handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================
