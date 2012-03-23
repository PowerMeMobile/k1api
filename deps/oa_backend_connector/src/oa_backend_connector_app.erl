-module(oa_backend_connector_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    oa_backend_connector_sup:start_link().

stop(_State) ->
    ok.
