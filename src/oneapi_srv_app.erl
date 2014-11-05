-module(oneapi_srv_app).

-behaviour(application).

%% application callbacks
-export([
    start/2,
    stop/1
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
