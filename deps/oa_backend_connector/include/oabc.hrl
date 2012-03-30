-ifndef(oabc_hrl).
-define(oabc_hrl, included).

-record(peer_spec, {
	id :: term(),
	type :: '2way' | 'forward' | 'backward',
	fw_q :: binary(),
	bw_q :: binary(),
	qprops = [] :: [{atom(), term()}],
	chan :: pid()
	}).

% -record(bind_req, {
% 	connectionId :: string(),
% 	remoteIp :: string(),
% 	customerId :: string(),
% 	user :: string(),
% 	password :: string(),
% 	type :: atom()
% 	% isCached     [6] BOOLEAN,
% 	% timestamp    [7] PreciseTime,
% 	% expiration   [8] PreciseTime
% 	}).

-endif. % oabc_hrl