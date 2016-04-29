-module(webturbine_route).

-export([matches_path/3,
         get_paths/1]).

-include("webturbine.hrl").

%%====================================================================
%% API
%%====================================================================

-spec matches_path(wtb_route(), [string()], [{atom(), string()}]) -> boolean() | {true, wtb_route()}.
matches_path(Route, PathTokens, PathInfo) ->
    RoutePaths = get_paths(Route),
    paths_match_path(RoutePaths, PathTokens, PathInfo).

-spec get_paths(wtb_route()) -> {wtb_route(), [[string() | atom()]]} | [[string() | atom()]].
get_paths(#wtb_route{routes=[], prefix=[], path=Paths}) ->
    Paths;
get_paths(#wtb_route{routes=[], prefix=Prefixes, path=[]}) ->
    Prefixes;
get_paths(#wtb_route{routes=[], prefix=Prefixes, path=Paths}) ->
    PrefixGroups = [ [ Prefix ++ Path || Path <- Paths ] || Prefix <- Prefixes ],
    lists:append(PrefixGroups);
get_paths(#wtb_route{routes=Routes, prefix=Prefixes, path=Paths}) ->
    TopRoutes = get_paths(
                  #wtb_route{routes=[], prefix=Prefixes, path=Paths}),
    SubRoutes = 
        lists:append(
          [ [{Route, get_paths(
              Route#wtb_route{
                prefix=TopRoutes ++ Route#wtb_route.prefix})}] || Route <- Routes ]),
    SubRoutes ++ TopRoutes.

%%====================================================================
%% Internal functions
%%====================================================================

-spec paths_match_path([[string() | atom()]], [string()], [{atom(), string()}]) -> boolean() | {true, wtb_route()}.
paths_match_path([], _, _) ->
    false;
paths_match_path([{Route, RoutePaths}|Rest], PathTokens, PathInfo) ->
    io:format("paths_match_path, ~p, ~p, ~p", [{Route, RoutePaths}, PathTokens, PathInfo]),
    case paths_match_path(RoutePaths, PathTokens, PathInfo) of
        true ->
            {true, Route};
        false ->
            paths_match_path(Rest, PathTokens, PathInfo)
    end;
paths_match_path([RoutePath|Rest], PathTokens, PathInfo) ->
    io:format("Path: ~p, Tokens: ~p, PathInfo: ~p.~n", [RoutePath, PathTokens, PathInfo]),
    case parts_match_path(RoutePath, PathTokens, PathInfo) of
        true ->
            true;
        false -> 
            paths_match_path(Rest, PathTokens, PathInfo)
    end.

-spec parts_match_path([string() | atom()], [string()], [{atom(), string()}]) -> boolean().
parts_match_path([], [], _) ->
    true;
parts_match_path(['*'], _, _) ->
    true;
parts_match_path([Rp|RouteParts], [Pt|PathTokens], PathInfo) when is_atom(Rp) ->
    case proplists:get_value(Rp, PathInfo) of
        undefined -> false;
        Pt -> parts_match_path(RouteParts, PathTokens, PathInfo);
        _ -> false
    end;
parts_match_path([Part|RouteParts], [Part|PathTokens], PathInfo) ->
    parts_match_path(RouteParts, PathTokens, PathInfo);
parts_match_path(_, _, _) ->
    false.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_paths_test() ->
    ?assertEqual([["one"]], 
                 get_paths(
                   wtb:route(one))),
    ?assertEqual([["one", "two"]], 
                 get_paths(
                   wtb:route(two, [["one", "two"]]))),
    ?assertEqual([["one", "three"],
                  ["one", "four"],
                  ["two", "three"],
                  ["two", "four"]],
                 get_paths(
                   #wtb_route{
                      prefix=[["one"], ["two"]], 
                      path=[["three"],["four"]]})),

    Inner1 = wtb:route(inner1),
    Inner2 = wtb:route(inner2),
    ?assertEqual([{Inner1,
                   [["outer1","middle1","inner1"],
                    ["outer1","middle2","inner1"],
                    ["outer2","middle1","inner1"],
                    ["outer2","middle2","inner1"]]},
                  {Inner2,
                   [["outer1","middle1","inner2"],
                    ["outer1","middle2","inner2"],
                    ["outer2","middle1","inner2"],
                    ["outer2","middle2","inner2"]]},
                  ["outer1","middle1"],
                  ["outer1","middle2"],
                  ["outer2","middle1"],
                  ["outer2","middle2"]], 
                 get_paths(
                   #wtb_route{
                      routes=[
                              Inner1,
                              Inner2
                             ],
                      prefix=[["outer1"],["outer2"]],
                      path=[["middle1"],["middle2"]]})).

path_match_test() ->
    ?assertEqual(true, parts_match_path(["hello"],["hello"],[])),
    ?assertEqual(true, parts_match_path(["hello", one],["hello", "one"],[{one, "one"}])),
    ?assertEqual(true, parts_match_path(["hello", '*'],["hello", "one", "two", "three"],[])),
    ?assertEqual(false, parts_match_path(["not", '*'],["hello", "one", "two", "three"],[])).

-endif.
