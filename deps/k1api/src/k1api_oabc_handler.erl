-module(k1api_oabc_handler).

-behaviour(oabc_bw_srv).

-export([handle_backward/2]).

-compile([{parse_transform, lager_transform}]).

-include("logging.hrl").

handle_backward(Id, Payload) ->
    ?log_debug("Id: ~p Payload: ~p", [Id, Payload]),
    ok.