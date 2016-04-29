-module(webturbine).

-export([dispatch/1, route_from_path/3]).

-include("webturbine.hrl").

%%====================================================================
%% API
%%====================================================================

-spec dispatch([module()]) -> [{[string() | atom], module(), [term()]}].
dispatch(Resources) ->
    WtbRoutes = [ {R, R:routes()} || R <- Resources ],
    WmRoutes = [ build_wm_routes(WtbR, []) || WtbR <- WtbRoutes ],
    lists:flatten(WmRoutes).

-spec route_from_path([wtb_route()], string(), [{atom(), string()}]) -> {error, not_found} | wtb_route().
route_from_path(Routes, Path, PathInfo) ->
    PathTokens = string:tokens(Path, "/"),

    %% Matches = [ R1 || R <- Routes,
    %%                   case webturbine_route:matches_path(R, PathTokens, PathInfo) of
    %%                       false -> false;
    %%                       {true, R1} -> true
    %%                   end ],
    
    Matches = lists:filtermap(
                fun(R) ->
                        Result = webturbine_route:matches_path(R, PathTokens, PathInfo),
                        io:format("Result: ~p~n", [Result]),
                        Result
                end,
                Routes),
    case Matches of
        [Route=#wtb_route{}|_] -> 
            Route;
        _ -> 
            {error, not_found}
    end.
        

%%====================================================================
%% Internal functions
%%====================================================================

build_wm_routes({_R, []}, Acc) ->
    lists:reverse(lists:flatten(Acc));
build_wm_routes({R, [#wtb_route{routes = [], prefix = [], path = Paths} | Rest]}, Acc) ->
    build_wm_routes({R, Rest}, [build_wm_route(R, [], Paths, []) | Acc]);
build_wm_routes({R, [#wtb_route{routes = [], prefix = Bases, path = []} | Rest]}, Acc) ->
    build_wm_routes({R, Rest}, [build_wm_route(R, [], Bases, []) | Acc]);
build_wm_routes({R, [#wtb_route{routes = [], prefix = Bases, path = Paths} | Rest]}, Acc) ->
    build_wm_routes({R, Rest}, [build_wm_routes(R, Bases, Paths, []) | Acc]);
build_wm_routes({R, [#wtb_route{routes = [SubRoute|SubRest], prefix = Bases, path = Paths}=Route | Rest]}, Acc) ->
    SubRoute1 = SubRoute#wtb_route{prefix = Bases ++ Paths ++ SubRoute#wtb_route.prefix},
    Route1 = Route#wtb_route{routes = SubRest},
    build_wm_routes({R, [Route1, SubRoute1 | Rest]}, Acc).

build_wm_routes(_R, [], _, Acc) ->
    Acc;
build_wm_routes(R, [Base|Rest], Paths, Acc) ->
    build_wm_routes(R, Rest, Paths, [build_wm_route(R, Base, Paths, [])|Acc]).

build_wm_route(_, _, [], Acc) ->
    Acc;
build_wm_route(R, Base, [Path|Rest], Acc) ->
    build_wm_route(R, Base, Rest, [{Base ++ Path, webturbine_router, [R]}|Acc]).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_route_test() ->
    R1 = wtb:route(hello),
    ?assertEqual(R1, route_from_path([R1],"hello",[])),
    R2 = wtb:route(node, [["nodes", node]]),
    R3 = #wtb_route{name = cluster,
                    path = [["clusters", cluster]],
                    routes = [
                              R2
                             ]},
    ?assertEqual(R2, route_from_path([R3],"clusters/cluster/nodes/hello",[{cluster, "cluster"},{node,"hello"}])).

-endif.
