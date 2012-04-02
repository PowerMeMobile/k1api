-module(oabc_req_sup).

-behaviour(supervisor).

-include("oabc.hrl").

%% API
-export([
	start_link/1,
	start_child/1
	]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Spec) ->
	supervisor:start_link(?MODULE, Spec).

start_child(Pid) ->
	supervisor:start_child(Pid, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Spec = #peer_spec{type = Type, id = ID, fw_q = QName}) ->
	Chan = oabc_amqp_pool:open_channel(),
	link(Chan),
	ok = oabc_amqp:queue_declare(Chan, QName, true, false, false),
	gproc:add_local_name({?MODULE, ID}),
	case Type of 
		'2way' ->
			{ok, { {simple_one_for_one, 5, 10}, [
				{ req_worker, {oabc_req_worker, start_link, [Spec#peer_spec{chan = Chan}]}, transient, brutal_kill, worker, [oabc_req_worker]}
			]} };
		fw ->
			{ok, { {simple_one_for_one, 5, 10}, [
				{ req_worker, {oabc_fw_worker, start_link, [Spec#peer_spec{chan = Chan}]}, transient, brutal_kill, worker, [oabc_fw_worker]}
			]} }
	end.