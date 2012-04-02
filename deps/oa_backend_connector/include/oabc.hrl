-ifndef(oabc_hrl).
-define(oabc_hrl, included).

-record(peer_spec, {
	id :: term(),
	type :: '2way' | 'forward' | 'backward',
	fw_q :: binary(),
	bw_q :: binary(),
	qprops = [] :: [{atom(), term()}],
	chan :: pid(),
	callback :: atom()
	}).

-endif. % oabc_hrl