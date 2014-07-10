-module(oneapi_srv_app).

-behaviour(application).

%% application callbacks
-export([
    start/2,
    prep_stop/1,
    stop/1
]).

%% Init API
-export([
    set_debug_level/0
]).

-include("application.hrl").
-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?log_info("k1api initializing...", []),
    Result = oneapi_srv_sup:start_link(),

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
