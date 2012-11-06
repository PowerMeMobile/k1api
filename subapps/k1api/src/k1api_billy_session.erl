-module(k1api_billy_session).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	get_session_id/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("application.hrl").
-include_lib("billy_common/include/logging.hrl").

-define(RECONNECT_TIMEOUT, 10000).

-record(state, {
	session_id :: any()
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_session_id() ->
	gen_server:call(?MODULE, {get_session_id}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, PropList} = application:get_env(?APP, billy_props),
	Host 		= proplists:get_value(host, PropList),
	Port 		= proplists:get_value(port, PropList),
	ClientId 	= proplists:get_value(username, PropList),
	ClientPw 	= proplists:get_value(password, PropList),

	case billy_client:start_session(Host, Port, ClientId, ClientPw) of
		{ok, SessionId} ->
			{ok, #state{session_id = SessionId}};
		{error, econnrefused} ->
			{ok, #state{}, ?RECONNECT_TIMEOUT};
		{error, invalid_credentials} ->
			{error, invalid_credentials}
	end.

handle_call({get_session_id}, _From, State = #state{session_id = undefined}) ->
	%% return the error and timeout instantly.
	{reply, {error, no_session}, State, 0};

handle_call({get_session_id}, _From, State = #state{session_id = SessionId}) ->
	{reply, {ok, SessionId}, State};

handle_call(_Request, _From, State = #state{}) ->
	{stop, bad_arg, State}.

handle_cast(_Msg, State) ->
	{stop, bad_arg, State}.

handle_info(timeout, State = #state{}) ->
	?log_info("Billy session timeout. Trying to reconnect...", []),
	{stop, no_session, State};

handle_info(_Info, State = #state{}) ->
	{stop, bad_arg, State}.

terminate(_Reason, _State = #state{session_id = undefined}) ->
	ok;

terminate(_Reason, _State = #state{session_id = SessionId}) ->
	ok = billy_client:stop_session(SessionId).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================
