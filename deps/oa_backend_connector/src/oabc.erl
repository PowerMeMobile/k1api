-module(oabc).

-compile([{parse_transform, lager_transform}]).
-export([
	request_backend_auth/2
	]).
-include("logging.hrl").
-include("oabc.hrl").

request_backend_auth(_, _) -> ok.
% request_backend_auth(#bind_req{
% 		connectionId = ConnId,
% 		remoteIp  = Ip,
% 		customerId = SysId,
% 		user = UserId,
% 		password = Password,
% 		type = Type
% 	}, Timeout) ->
% 	Now = oabc_time:milliseconds(),
%     Then = Now + Timeout,
%     Timestamp = #'PreciseTime'{time = oabc_time:utc_str(oabc_time:milliseconds_to_now(Now)),
%                                milliseconds = Now rem 1000},
%     Expiration = #'PreciseTime'{time = oabc_time:utc_str(oabc_time:milliseconds_to_now(Then)),
%                                 milliseconds = Then rem 1000},
%     BindRequest = #'BindRequest'{
%         connectionId = "",
%         remoteIp     = "",
%         customerId   = SysId,
%         userId       = UserId,
%         password     = Password,
%         type         = Type,
%         isCached     = false,
%         timestamp    = Timestamp,
%         expiration   = Expiration
%     },
%     {ok, Encoded} = 'FunnelAsn':encode('BindRequest', BindRequest),
%     Payload = list_to_binary(Encoded),
%     RoutingKey = funnel_app:get_env(queue_backend_auth),
%     Props = #'P_basic'{
%         content_type = <<"BindRequest">>,
%         delivery_mode = 2,
%         message_id   = uuid:unparse(uuid:generate()),
%         reply_to     = funnel_app:get_env(srv_control_queue)
%     },
%     fun_amqp:basic_publish(Chan, RoutingKey, Payload, Props).




% request_backend_auth(Chan, UUID, Addr, CustomerId, UserId, Password, Type, Timeout) ->
%     Cached = temp_fun_cache:fetch({CustomerId, UserId, Type, Password}),
%     Now = oabc_time:milliseconds(),
%     Then = Now + Timeout,
%     Timestamp = #'PreciseTime'{time = fun_time:utc_str(fun_time:milliseconds_to_now(Now)),
%                                milliseconds = Now rem 1000},
%     Expiration = #'PreciseTime'{time = fun_time:utc_str(fun_time:milliseconds_to_now(Then)),
%                                 milliseconds = Then rem 1000},
%     BindRequest = #'BindRequest'{
%         connectionId = UUID,
%         remoteIp     = Addr,
%         customerId   = CustomerId,
%         userId       = UserId,
%         password     = Password,
%         type         = Type,
%         isCached     = Cached =/= not_found,
%         timestamp    = Timestamp,
%         expiration   = Expiration
%     },
%     {ok, Encoded} = 'FunnelAsn':encode('BindRequest', BindRequest),
%     Payload = list_to_binary(Encoded),
%     RoutingKey = funnel_app:get_env(queue_backend_auth),
%     Props = #'P_basic'{
%         content_type = <<"BindRequest">>,
%         delivery_mode = 2,
%         message_id   = uuid:unparse(uuid:generate()),
%         reply_to     = funnel_app:get_env(queue_server_control)
%     },
%     fun_amqp:basic_publish(Chan, RoutingKey, Payload, Props).