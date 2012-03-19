-module(k1api_app).

-behaviour(application).

-compile([{parse_transform, lager_transform}]).

-include("logging.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?log_info("initialization...", []),
	% Start eoneapi listener
	EOneAPIProps = [
		{port, 8080},
		{sms_handler, k1api_sms_handler}
	],
	eoneapi:start_service(EOneAPIProps),
    k1api_sup:start_link().

stop(_State) ->
    ok.
