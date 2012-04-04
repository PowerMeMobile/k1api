-module(oa_backend_connector_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, _Value} = application:get_env(app_id), %% validation of app_id value existing
    oabc_sup:start_link().

stop(_State) ->
    ok.
