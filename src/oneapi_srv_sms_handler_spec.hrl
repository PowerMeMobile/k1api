-ifndef(oneapi_srv_sms_handler_spec_hrl).
-define(oneapi_srv_sms_handler_spec_hrl, included).
-include("oneapi_srv.hrl").

-spec init(credentials()) ->
    {ok, state()} |
    {error, term()}.

-spec handle_send_outbound(outbound_sms(), state()) ->
    {ok, request_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_query_delivery_status(sender_address(), request_id(), state()) ->
    {ok, sms_delivery_statuses()}  |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_subscribe_delivery_notifications(subscribe_delivery_notifications(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_unsubscribe_delivery_notifications(sender_address(), subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_retrieve_inbound(retrieve_sms_req(), state()) ->
    {ok, [inbound_sms()], pending_sms()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_subscribe_inbound_notifications(subscribe_inbound(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_unsubscribe_inbound_notifications(subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-endif. % oneapi_srv_sms_handler_spec_hrl
