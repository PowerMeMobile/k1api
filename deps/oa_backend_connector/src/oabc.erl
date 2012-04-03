-module(oabc).

%% To do
%% -cast
%% -async bw
%% -qos
%% -monitors

%% FOR TEST
-behaviour(oabc_bw_srv).
-export([handle_backward/2]).
%%
-compile([{parse_transform, lager_transform}]).
-export([
    register_2way/3,
    register_2way/4,
    register_fw/2,
    register_fw/3,
    register_bw/3,
    register_bw/4,
    call/3,
    call/2,
    init/0,
    test/1
	]).
-include("logging.hrl").
-include("oabc.hrl").
%% For tests
handle_backward(Id, Payload) ->
    ?log_debug("Id: ~p Payload: ~p", [Id, Payload]),
    ok.
%%
register_2way(Id, QNameReq, QNameResp) ->
    register_2way(Id, QNameReq, QNameResp, []).
register_2way(Id, QNameReq, QNameResp, Props) ->
    register(Id, '2way', QNameReq, QNameResp, Props).

register_fw(Id, QNameFw)->
    register_fw(Id, QNameFw, []).
register_fw(Id, QNameFw, Props)->
    register(Id, fw, QNameFw, <<>>, Props).

register_bw(Id, QNameBw, CallBackModule) when is_atom(CallBackModule)->
    register_bw(Id, QNameBw, CallBackModule, []).
register_bw(Id, QNameBw, CallBackModule, Props) when is_atom(CallBackModule) ->
    register(Id, bw, <<>>, QNameBw, CallBackModule, Props).

register(Id, Type, QNameReq, QNameResp, QProps) ->
    register(Id, Type, QNameReq, QNameResp, undefined, QProps).

register(Id, Type, QNameReq, QNameResp, CallBackModule, QProps) ->
    oabc_peers_sup:start_child(#peer_spec{
                                    id = Id,
                                    type = Type,
                                    fw_q = QNameReq,
                                    bw_q = QNameResp,
                                    callback = CallBackModule,
                                    qprops = QProps}).

%% TEST

init() ->
    oabc:register_2way(auth, <<"pmm.k1api.auth">>, <<"pmm.k1api.auth">>),
    ?log_debug("ok", []),
    oabc:register_fw(submit_sms, <<"pmm.k1api.test">>),
    ?log_debug("ok", []),
    oabc:register_bw(receipts, <<"pmm.k1api.test">>, oabc),
    ?log_debug("ok", []).

test('2way') ->
    Response = oabc:call(auth, <<"hello">>),
    io:format("Response: ~p~n", [Response]);
test(fw) ->
    oabc:call(submit_sms, <<"hello">>).


%%%%%%%%%%

call(Id, Payload) ->
    call(Id, Payload, 5000).
call(Id, Payload, Timeout)->
    Result = gproc:lookup_local_name({oabc_req_sup, Id}),
    ?log_debug("oabc_req_sup: ~p", [Result]),
    case Result of
        Pid when is_pid(Pid) ->
            {ok, WorkerPid} = oabc_req_sup:start_child(Pid),
            ?log_debug("worker pid: ~p", [WorkerPid]),
            CallResult = gen_server:call(WorkerPid, {send, Payload}, Timeout),
            ?log_debug("CallResult: ~p", [CallResult]);
        _ -> {error, no_proc}
    end.