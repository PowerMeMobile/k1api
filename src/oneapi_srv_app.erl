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

-include_lib("alley_common/include/application_spec.hrl").

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

-spec set_debug_level() -> ok.
set_debug_level() ->
    ok = application:ensure_started(sync),
    lager:set_loglevel(lager_console_backend, debug).
