-ifndef(oneapi_srv_sms_handler_spec_hrl).
-define(oneapi_srv_sms_handler_spec_hrl, included).
-include("oneapi_srv.hrl").

-spec init(credentials()) ->
    {ok, state()} |
    {error, term()}.

-spec handle_send_outbound(outbound_sms(), state()) ->
    {ok, request_id()} |
    {error, term()}.

-spec handle_query_delivery_status(sender_address(), request_id(), state()) ->
    {ok, sms_delivery_statuses()}  |
    {error, term()}.

-spec handle_subscribe_to_delivery_notifications(subscribe_delivery_notifications(), state()) ->
    {ok, subscription_id()} |
    {error, term()}.

-spec handle_unsubscribe_from_delivery_notifications(subscription_id(), state()) ->
    {ok, deleted} |
    {error, term()}.

-spec handle_retrieve_inbound(retrieve_sms_req(), state()) ->
    {ok, [inbound_sms()], pending_sms()} |
    {error, term()}.

-spec handle_subscribe_to_inbound_notifications(subscribe_inbound(), state()) ->
    {ok, subscription_id()} |
    {error, term()}.

-spec handle_unsubscribe_from_inbound_notifications(subscription_id(), state()) ->
    {ok, deleted} |
    {error, term()}.

-endif. % oneapi_srv_sms_handler_spec_hrl
