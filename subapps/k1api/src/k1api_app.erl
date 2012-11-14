-module(k1api_app).

-behaviour(application).

-include("logging.hrl").

%% Application callbacks
-export([
    start/2,
    prep_stop/1,
    stop/1
    ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?log_info("k1api initializing...", []),

    Result = k1api_sup:start_link(),

	EOneAPIProps = [
		{port, 8081},
		{sms_handler, k1api_sms_handler}
	],
	eoneapi:start_service(EOneAPIProps),

	Result.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.
