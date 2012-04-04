-module(ktest).
-compile([{parse_transform, lager_transform}]).
-include("logging.hrl").
-behaviour(oabc_bw_srv).
-export([
    init/0,
    start/0
    ]).
-export([
    handle_backward/2
    ]).

handle_backward(Id, Payload) ->
    ?log_debug("Id: ~p Payload: ~p", [Id, Payload]),
    ok.

init() ->
    Props = [{durable,false},{exclusive,true},{auto_delete,true}],
    oabc:register_2way('2way', <<"pmm.k1api.test2way">>, <<"pmm.k1api.test2way">>, Props),
    ?log_debug("register_2way ok", []),
    oabc:register_fw(submit_sms, <<"pmm.k1api.fwbw_test">>, Props),
    ?log_debug("register_fw ok", []),
    oabc:register_bw(receipts, <<"pmm.k1api.fwbw_test">>, ?MODULE, Props),
    ?log_debug("register_bw ok", []).

start() ->
    ?log_debug("2 way call", []),
    MsgId = oabc_uuid:newid(),
    Response = oabc:call('2way', <<"hello">>, [{message_id, MsgId}, {correlation_id, MsgId}]),
    ?log_debug("Response: ~p", [Response]),

    ?log_debug("1 way call", []),
    Result2 = oabc:call(submit_sms, <<"hello">>),
    ?log_debug("result: ~p", [Result2]).