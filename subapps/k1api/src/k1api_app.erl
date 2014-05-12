-module(k1api_app).

-behaviour(application).

-include("logging.hrl").
-include("application.hrl").

%% Application callbacks
-export([
    start/2,
    prep_stop/1,
    stop/1
]).

%% Init
-export([
    set_debug_level/0
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?log_info("k1api initializing...", []),
    Result = k1api_sup:start_link(),

    {ok, Addr} = application:get_env(?APP, http_addr),
    {ok, Port} = application:get_env(?APP, http_port),
    {ok, AcceptorsNum} = application:get_env(?APP, http_acceptors_num),

    EOneAPIProps = [
        {addr, Addr},
        {port, Port},
        {acceptors_num, AcceptorsNum},
        {sms_handler, k1api_sms_handler}
    ],
    eoneapi:start_service(EOneAPIProps),
    Result.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.

%% ===================================================================
%% Init API
%% ===================================================================

set_debug_level() ->
    lager:set_loglevel(lager_console_backend, debug).
