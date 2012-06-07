-module(k1api_subscription_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% TODO
%% add confirms

-export([
	start_link/0,
	subscribe/1,
	unsubscribe/1
	]).

-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-include("FunnelAsn.hrl").
-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eoneapi/include/eoneapi.hrl").
-include("gen_server_spec.hrl").

-record(state, {
	chan :: pid()
}).

%% API Functions Definitions

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec subscribe(Payload :: binary()) -> ok | {error, Error :: term()}.
subscribe(Payload) ->
	{ok, Chan} = get_channel(),
	{ok, AuthRequestQ} = application:get_env(k1api, subscriptions_q),
    Props = #'P_basic'{
        content_type = <<"subscribeevent">>,
        message_id   = k1api_uuid:bin_id(),
		delivery_mode = 2
    },
    k1api_amqp_funs:basic_publish(Chan, AuthRequestQ, Payload, Props).

-spec unsubscribe(Payload :: binary()) -> ok.
unsubscribe(Payload) -> ok.

%% GenServer Callback Functions Definitions

init([]) ->
	Chan = k1api_amqp_pool:open_channel(),
	link(Chan),

	% declare subscribe queue
	{ok, RequestQ} = application:get_env(subscriptions_q),
	ok = k1api_amqp_funs:queue_declare(Chan, RequestQ),

	{ok, #state{chan = Chan}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------------------------------------------------------------------
% Internal Function Definitions
% ------------------------------------------------------------------

get_channel() ->
	gen_server:call(?MODULE, get_channel, 5000).
