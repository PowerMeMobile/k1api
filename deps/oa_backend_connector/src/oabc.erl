-module(oabc).

%% behaviour test
-behaviour(oabc_bw_srv).
-export([handle_backward/2]).
%%%%%%%%%%%

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
    cast/2
    ]).

-export([
    init/0,
    test/1
	]).

-include("logging.hrl").
-include("oabc.hrl").

%%%%%%%%%%%%%%%%%
%% behaviour test
%%%%%%%%%%%%%%%%%

handle_backward(Id, Payload) ->
    ?log_debug("Id: ~p Payload: ~p", [Id, Payload]),
    ok.

%%%%%%%%%%%%%%%
%% TEST SECTION
%%%%%%%%%%%%%%%

init() ->
    oabc:register_2way(auth, <<"pmm.k1api.auth">>, <<"pmm.k1api.auth">>),
    ?log_debug("ok", []),
    oabc:register_fw(submit_sms, <<"pmm.k1api.test">>),
    ?log_debug("ok", []),
    oabc:register_bw(receipts, <<"pmm.k1api.test">>, oabc),
    ?log_debug("ok", []).

test('2way') ->
    Response = oabc:call(auth, <<"hello">>),
    ?log_debug("Response: ~p", [Response]);
test(fw) ->
    oabc:call(submit_sms, <<"hello">>).

%%%%%%%%%%%%%%%%%%%
%% END TEST SECTION
%%%%%%%%%%%

register_2way(Id, QNameReq, QNameResp) ->
    register_2way(Id, QNameReq, QNameResp, []).
register_2way(Id, QNameReq, QNameResp, Props) ->
    register(Id, '2way', QNameReq, QNameResp, Props).

register_fw(Id, QNameFw)->
    register_fw(Id, QNameFw, []).
register_fw(Id, QNameFw, Props)->
    register(Id, forward, QNameFw, <<>>, Props).

register_bw(Id, QNameBw, CallBackModule) when is_atom(CallBackModule)->
    register_bw(Id, QNameBw, CallBackModule, []).
register_bw(Id, QNameBw, CallBackModule, Props) when is_atom(CallBackModule) ->
    register(Id, backward, <<>>, QNameBw, CallBackModule, Props).

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

%%%%%%%%%%%%%%%%%%%

call(Id, Payload) ->
    call(Id, Payload, 5000).
call(Id, Payload, Timeout)->
    Result = gproc:lookup_local_name({oabc_fw_srv, Id}),
    ?log_debug("oabc_fw_srv: ~p", [Result]),
    case Result of
        Srv when is_pid(Srv) ->
            CallResult = gen_wp:call(Srv, {send, Payload}, Timeout),
            ?log_debug("CallResult: ~p", [CallResult]),
            CallResult;
        _ -> {error, no_proc}
    end.

cast(Id, Payload) ->
    Result = gproc:lookup_local_name({oabc_fw_srv, Id}),
    ?log_debug("oabc_fw_srv: ~p", [Result]),
    case Result of
        Srv when is_pid(Srv) ->
            gen_wp:call(Srv, {send, Payload}),
            ?log_debug("gen_wp:cast", []),
            ok;
        _ -> ok
    end.