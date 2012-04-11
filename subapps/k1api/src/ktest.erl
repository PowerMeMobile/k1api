-module(ktest).
-compile([{parse_transform, lager_transform}]).
-include("logging.hrl").
-include_lib("oa_proto/include/oa_pb.hrl").
-behaviour(oabc_bw_srv).
-export([
    test/0
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

test() ->
    init(),
    call_test().
    % serverUpEvent_test(),
    % authReq_test(),
    % authResponse_test().

call_test() ->    
    ?log_debug("2 way call", []),
    MsgId = oabc_uuid:newid(),
    Response = oabc:call('2way', <<"hello">>, [{message_id, MsgId}, {correlation_id, MsgId}]),
    ?log_debug("Response: ~p", [Response]),

    ?log_debug("1 way call", []),
    Result2 = oabc:call(submit_sms, <<"hello">>),
    ?log_debug("result: ~p", [Result2]).

% serverUpEvent_test() ->
%     ?log_info("server up event", []),
%     ServerUpEvent = #frontendupevent{
%         reset_subscriptions = false,
%         auth_q = <<"auth.q">>,
%         register_q = <<"register.q">>,
%         control_q = <<"control.q">>},
%     ServerUpEventProto = oa_pb:encode_frontendupevent(ServerUpEvent),
%     ServerUpEvent = oa_pb:decode_frontendupevent(ServerUpEventProto).
    
% authReq_test() ->
%     ?log_info("~nauth req", []),
%     AuthReq = #authreq{
%         system_id = "sysid",
%         user_id = "userid",
%         password = "password",
%         type = transmitter,
%         is_cached = false,
%         timestamp = 20120404
%         },
%     AuthReqProto = oa_pb:encode_authreq(AuthReq),
%     AuthReq = oa_pb:decode_authreq(AuthReqProto).

% authResponse_test() ->
%     ?log_debug("authResponse test", []),
%     Addr = #addr{
%         addr = "addr",
%         ton = 0, 
%         npi = 0
%     },
%     Provider = #provider{
%         id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
%         gateway = <<164,126,21,42,105,198,74,14,164,95,243,58,84,205,60,178>>,
%         bulk_gateway = <<48,90,54,196,53,209,68,152,135,237,158,12,18,39,14,50>>,
%         receipts_supported = true
%     },

%     Network = #network{
%         id = <<254,63,67,15,85,215,73,109,134,188,4,232,0,90,248,228>>,
%         country_code = "375",
%         numbers_len = 7,
%         prefixes = ["33", "44"],
%         provider_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>
%     },

%     Customer = #customer{
%         id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
%         uuid = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
%         priority = 1,
%         rps = 100,
%         allowed_sources = [Addr, Addr],
%         default_source = Addr,
%         networks = [Network],
%         providers = [Provider],
%         default_provider_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
%         receipts_allowed = true,
%         no_retry = true,
%         default_validity = <<>>,
%         max_validity = 700000
%     },

%     AuthResp = #authresponse{
%         result = customer,
%         customer = Customer,
%         error = ""
%     },

%     AuthRespProto = oa_pb:encode_authresponse(AuthResp),
%     AuthResp = oa_pb:decode_authresponse(AuthRespProto).