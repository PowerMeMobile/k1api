-module(oneapi_srv_incoming_srv).

-behaviour(gen_server).
-behaviour(cowboy_http_handler).

%% API
-export([
    start/0,
    give_sms/0,
    give_receipts/0
]).

%% Cowboy Callbacks
-export([
    init/3,
    handle/2,
    terminate/3
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
    sms = []      :: binary(),
    receipts = [] :: binary()
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
    {Method, _} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            {ok, Body, _} = cowboy_req:body(Req),
            save_receipt(Body),
            {ok, Req2} = cowboy_req:reply(201, [], <<"Created">>, Req),
            {ok, Req2, State};
        _Any ->
            {ok, Req2} = cowboy_req:reply(400, [], <<"Improper method">>, Req),
            {ok, Req2, State}
    end;
handle(Req, State = incoming_sms) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            {ok, Body, _} = cowboy_req:body(Req),
            save_sms(Body),
            {ok, Req2} = cowboy_req:reply(201, [], <<"Created">>, Req),
            {ok, Req2, State};
        _Any ->
            {ok, Req2} = cowboy_req:reply(400, [], <<"Improper method">>, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
     ok.

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),

    TransportOpts = [{port, ?PORT}],
    Dispatch = cowboy_router:compile([
        %% {Host, list({Path, Handler, Opts})}
        {'_', [
            %% REST API
            {"/incoming_sms", ?MODULE, [incoming_sms]},
            {"/receipts", ?MODULE, [receipts]}
        ]}
    ]),
    ProtocolOpts = [
        {env, [{dispatch, Dispatch}]}
    ],

    {ok, _Pid} = cowboy:start_http(
        my_http_listener, 1, TransportOpts, ProtocolOpts
    ),
    {ok, #st{}}.

handle_call(give_sms, _From, State = #st{sms = Sms}) ->
    {reply, {ok, Sms}, State};
handle_call(give_receipts, _From, State = #st{receipts = Receipts}) ->
    {reply, {ok, Receipts}, State};
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
