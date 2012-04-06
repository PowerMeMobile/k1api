-module(oa_pb_tests).
-include_lib("oa_proto/include/oa_pb.hrl").
-include_lib("eunit/include/eunit.hrl").

serverUpEvent_test() ->
    ServerUpEvent = #frontendupevent{
        reset_subscriptions = false,
        auth_q = <<"auth.q">>,
        register_q = <<"register.q">>,
        control_q = <<"control.q">>},
    ?assertEqual(ServerUpEvent, oa_pb:decode_frontendupevent(oa_pb:encode_frontendupevent(ServerUpEvent))).
    
authReq_test() ->
    AuthReq = #authreq{
        system_id = "sysid",
        user_id = "userid",
        password = "password",
        type = transmitter,
        is_cached = false,
        timestamp = 20120404
        },
    ?assertEqual(AuthReq, oa_pb:decode_authreq(oa_pb:encode_authreq(AuthReq))).

authResponse_test() ->
    Addr = #addr{
        addr = "addr",
        ton = 0, 
        npi = 0
    },
    Provider = #provider{
        id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        gateway = <<164,126,21,42,105,198,74,14,164,95,243,58,84,205,60,178>>,
        bulk_gateway = <<48,90,54,196,53,209,68,152,135,237,158,12,18,39,14,50>>,
        receipts_supported = true
    },

    Network = #network{
        id = <<254,63,67,15,85,215,73,109,134,188,4,232,0,90,248,228>>,
        country_code = "375",
        numbers_len = 7,
        prefixes = ["33", "44"],
        provider_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>
    },

    Customer = #customer{
        id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        uuid = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        priority = 1,
        rps = 100,
        allowed_sources = [Addr, Addr],
        default_source = Addr,
        networks = [Network],
        providers = [Provider],
        default_provider_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        receipts_allowed = true,
        no_retry = true,
        default_validity = <<>>,
        max_validity = 700000
    },

    AuthResp = #authresponse{
        result = customer,
        customer = Customer,
        error = ""
    },
    ?assertEqual(AuthResp, oa_pb:decode_authresponse(oa_pb:encode_authresponse(AuthResp))).

subscribeEvent_test() ->
    SubscribeEvent = #subscribeevent{
        subscribe_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        queue_name = <<"queue_name">>,
        customer_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        user_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>},
    ?assertEqual(SubscribeEvent, oa_pb:decode_subscribeevent(oa_pb:encode_subscribeevent(SubscribeEvent))).    

unsubscribeEvent_test() ->
    UnsubscribeEvent = #unsubscribeevent{
        subscribe_id = <<66,188,127,115,164,36,74,195,164,230,176,105,129,84,18,162>>,
        reason = "normal"
    },
    ?assertEqual(UnsubscribeEvent, oa_pb:decode_unsubscribeevent(oa_pb:encode_unsubscribeevent(UnsubscribeEvent))).