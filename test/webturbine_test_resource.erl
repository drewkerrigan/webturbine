-module(webturbine_test_resource).

-behaviour(webturbine_resource).

-export([routes/0, echo_get/1]).

-include("webturbine.hrl").

routes() ->
    [#wtb_route{name = echo, path = [["echo",this]]}].

echo_get(Req) ->
    V = wrq:path_info(this, Req),
    V.
