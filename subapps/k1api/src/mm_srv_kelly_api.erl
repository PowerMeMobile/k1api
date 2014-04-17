-module(mm_srv_kelly_api).

-ignore_xref([{start_link, 0}]).

%% API
-export([
    start_link/0
]).

-export([
    get_coverage/3,
    get_delivery_status/3,
    get_delivery_status/4
]).

-include("logging.hrl").
-include("application.hrl").
-include_lib("alley_dto/include/adto.hrl").

-type customer_id() :: binary().
-type user_id()     :: binary().
-type version()     :: binary().
-type request_id()  :: binary().
-type sender_addr() :: binary().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, QueueName} = application:get_env(?APP, kelly_api_queue),
    rmql_rpc_client:start_link(?MODULE, QueueName).

-spec get_coverage(customer_id(), user_id(), version()) ->
    {ok, [#k1api_coverage_response_dto{}]} | {error, timeout}.
get_coverage(CustomerId, UserId, Version) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #k1api_coverage_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        version = Version
    },
    ?log_debug("Sending coverage request: ~p", [Req]),
    {ok, Payload} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, Payload, <<"CoverageReq">>) of
        {ok, Bin} ->
            case adto:decode(#k1api_coverage_response_dto{}, Bin) of
                {ok, Resp = #k1api_coverage_response_dto{}} ->
                    ?log_debug("Got coverage response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Coverage response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_debug("Got coverage response: timeout", []),
            {error, timeout}
    end.

-spec get_delivery_status(customer_id(), user_id(), request_id()) ->
    {ok, [#k1api_sms_delivery_status_response_dto{}]} | {error, timeout}.
get_delivery_status(CustomerId, UserId, SmsReqId) ->
    get_delivery_status(CustomerId, UserId, SmsReqId, <<>>).

-spec get_delivery_status(customer_id(), user_id(), request_id(), sender_addr()) ->
    {ok, [#k1api_sms_delivery_status_response_dto{}]} | {error, timeout}.
get_delivery_status(CustomerId, UserId, SmsReqId, SenderAddr) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #k1api_sms_delivery_status_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        sms_request_id = SmsReqId,
        address = soap_srv_utils:addr_to_dto(SenderAddr)
    },
    ?log_debug("Sending delivery status request: ~p", [Req]),
    {ok, Payload} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, Payload, <<"DeliveryStatusReq">>) of
        {ok, Bin} ->
            case adto:decode(#k1api_sms_delivery_status_response_dto{}, Bin) of
                {ok, Resp = #k1api_sms_delivery_status_response_dto{}} ->
                    ?log_debug("Got delivery status response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Delivery status response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_debug("Got delivery status response: timeout", []),
            {error, timeout}
    end.
