-module(webturbine).

-export([dispatch/1]).

-include("webturbine.hrl").

%%====================================================================
%% API
%%====================================================================

-spec dispatch([module()]) -> [{[string() | atom], module(), [term()]}].
dispatch(Resources) ->
    Routes =
        [ [ [ {Path, webturbine_handler, [Route1]}
              || {Route1, Path} <- wtb_route:generate_paths(R, Route) ]
            || Route <- Routes ]
          || {R, Routes} <- [ {R, R:routes()} || R <- Resources ] ],
    [ {route_to_cb_route(Parts, ""), M, A} ||
        {Parts, M, A} <- lists:flatten(Routes) ].

%%====================================================================
%% Internal functions
%%====================================================================

route_to_cb_route([], Accum) ->
    Accum;
route_to_cb_route([Part|Rest], Accum) when is_atom(Part) ->
    route_to_cb_route(Rest, Accum ++ "/[:" ++ atom_to_list(Part) ++ "]");
route_to_cb_route(['*'|Rest], Accum) ->
    route_to_cb_route(Rest, Accum ++ "/[...]");
route_to_cb_route([Part|Rest], Accum) ->
    route_to_cb_route(Rest, Accum ++ "/" ++ Part).
