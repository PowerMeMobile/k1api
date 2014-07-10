-ifndef(eoneapi_sms_hrl).
-define(eoneapi_sms_hrl, included).
-include("eoneapi.hrl").

-spec init(credentials()) ->
    {ok, state()} |
    {error, denied}.

-spec handle_send_sms_req(outbound_sms(), state()) ->
    {ok, request_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_delivery_status_req(sender_address(), request_id(), state()) ->
    {ok, sms_delivery_statuses()}  |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_delivery_notifications_subscribe(delivery_receipt_subscribe(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_delivery_notifications_unsubscribe(sender_address(), subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_retrieve_req(retrieve_sms_req(), state()) ->
    {ok, [inbound_sms()], pending_sms()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_inbound_subscribe(subscribe_inbound(), state()) ->
    {ok, subscription_id()} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-spec handle_inbound_unsubscribe(subscription_id(), state()) ->
    {ok, deleted} |
    {exception, exception()} |
    {exception, exception(), excep_params()}.

-endif. % eoneapi_sms_hrl
