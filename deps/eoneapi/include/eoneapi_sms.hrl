-ifndef(eoneapi_sms_hrl).
-define(eoneapi_sms_hrl, included).
-include("eoneapi.hrl").

%% credentials check

-spec init(credentials()) ->
	{ok, state()} |
	{error, denied} |
	{error, error()}.

%% Send an SMS

-spec handle_send_sms_req(credentials(), outbound_sms(), state()) ->
	{ok, request_id()} |
	{error, denied}.
	%  |
	% {error, already_sent}. % by client collerator

% Query the delivery status of an SMS

-spec handle_delivery_status_req(credentials(), sender_address(), request_id(), state()) ->
	{ok, sms_delivery_statuses()}  |
	{error, denied}.

% Subscribe to SMS delivery notifications

-spec handle_delivery_notifications_subscribe(credentials(), del_rec_subscribe(), state()) ->
	{ok, subscription_id()} |
	{error, denied}.

-spec handle_delivery_notifications_unsubscribe(credentials(), sender_address(), subscription_id(), state()) ->
	{ok, deleted} |
	{error, denied}.

%% Retrieve messages sent to your Web application

-spec handle_retrieve_req(credentials(), retrieve_sms_req(), state()) ->
	{ok, [inbound_sms()], pending_sms()} |
	{error, denied}.

%% Subscribe to notifications of messages sent to your application

-spec handle_inbound_subscribe(credentials(), subscribe_inbound(), state()) ->
	{ok, subscription_id()} |
	{error, denied}.

%% Stop the subscription to message notifications

-spec handle_inbound_unsubscribe(credentials(), subscription_id(), state()) ->
	{ok, deleted} |
	{error, denied}.

-endif. % eoneapi_sms_hrl