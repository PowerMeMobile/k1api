-module(oneapi_srv_app).

-behaviour(application).

%% application callbacks
-export([
    start/2,
    stop/1
]).

%% Init API
-export([
    set_debug_level/0
]).

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Result = oneapi_srv_sup:start_link(),
    oneapi_srv_protocol:init(),
    Result.

stop(_State) ->
    ok.

%% ===================================================================
%% Init API
%% ===================================================================

set_debug_level() ->
    lager:set_loglevel(lager_console_backend, debug).
