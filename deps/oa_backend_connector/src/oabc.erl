-module(oabc).

-compile([{parse_transform, lager_transform}]).

-include("logging.hrl").
-include("oabc.hrl").

-export([
    register_2way/3,
    register_2way/4,
    register_fw/2,
    register_fw/3,
    register_bw/3,
    register_bw/4,
    call/4,
    call/3,
    call/2,
    cast/3,
    cast/2
    ]).


register_2way(Id, QNameReq, QNameResp) ->
    register_2way(Id, QNameReq, QNameResp, []).
register_2way(Id, QNameReq, QNameResp, Props) ->
    register(Id, '2way', QNameReq, QNameResp, Props). %% Props will be apply to both queues

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

register(Id, Type, QNameReq, QNameResp, CallBackModule, QProps) -> %% in case of using same id it causes problems
    oabc_peers_sup:start_child(#peer_spec{
                                    id = Id,
                                    type = Type,
                                    fw_q = QNameReq,
                                    bw_q = QNameResp,
                                    callback = CallBackModule,
                                    qprops = QProps}).


call(Id, Payload) ->
    call(Id, Payload, 5000).
call(Id, Payload, Props) when is_list(Props)->
    call(Id, Payload, 5000, Props);
call(Id, Payload, Timeout) when is_integer(Timeout)->
    call(Id, Payload, Timeout, []).
call(Id, Payload, Timeout, Props) when is_integer(Timeout)->
    Result = gproc:lookup_local_name({oabc_fw_srv, Id}),
    % ?log_debug("oabc_fw_srv: ~p", [Result]),
    case Result of
        Srv when is_pid(Srv) ->
            CallResult = gen_wp:call(Srv, {send, Payload, Props}, Timeout),
            % ?log_debug("CallResult: ~p", [CallResult]),
            CallResult;
        _ -> {error, no_proc}
    end.

cast(Id, Payload) ->
    cast(Id, Payload, []).
cast(Id, Payload, Props) ->
    Result = gproc:lookup_local_name({oabc_fw_srv, Id}),
    % ?log_debug("oabc_fw_srv: ~p", [Result]),
    case Result of
        Srv when is_pid(Srv) ->
            gen_wp:cast(Srv, {send, Payload, Props}),
            % ?log_debug("gen_wp:cast", []),
            ok;
        _ -> ok
    end.