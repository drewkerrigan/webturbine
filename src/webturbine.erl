-module(webturbine).

-export([dispatch_wm/1,
         dispatch_cb/1]).

-include("webturbine.hrl").

%%====================================================================
%% API
%%====================================================================

-spec dispatch_wm([module()]) -> [{[string() | atom], module(), [term()]}].
dispatch_wm(Resources) ->
    WMRoutes = 
        [ [ [ {Path, webturbine_handler, [Route1]} 
              || {Route1, Path} <- wtb_route:generate_paths(R, Route) ]
            || Route <- Routes ]
          || {R, Routes} <- [ {R, R:routes()} || R <- Resources ] ],
    lists:flatten(WMRoutes).

-spec dispatch_cb([module()]) -> [{[string() | atom], module(), [term()]}].
dispatch_cb(Resources) ->
    WMRoutes = dispatch_wm(Resources),
    [ {wm_route_to_cb_route(Parts, ""), M, A} ||
        {Parts, M, A} <- WMRoutes ].

%%====================================================================
%% Internal functions
%%====================================================================

wm_route_to_cb_route([], Accum) ->
    Accum;
wm_route_to_cb_route([Part|Rest], Accum) when is_atom(Part) ->
    Part1 = atom_to_list(Part),
    wm_route_to_cb_route(Rest, Accum ++ "/[:" ++ Part1 ++ "]");
wm_route_to_cb_route(['*'|Rest], Accum) ->
    wm_route_to_cb_route(Rest, Accum ++ "/[...]");
wm_route_to_cb_route([Part|Rest], Accum) ->
    wm_route_to_cb_route(Rest, Accum ++ "/" ++ Part).
