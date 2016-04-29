-module(wtb_route).

-export([new/1, new/2, new/3]).
-export([handler/4]).
-export([static/2, static/3]).

-export([get_field/2,
         set_field/3]).

-export([find_from_path/3,
         matches_path/3,
         generate_flat_paths/1,
         generate_paths/1]).

-include("webturbine.hrl").

%%====================================================================
%% API
%%====================================================================

-spec new(wtb_route_name()) -> wtb_route().
new(Name) ->
    #wtb_route{name=Name, path=[[atom_to_list(Name)]]}.

-spec new(wtb_route_name(), [wtb_route_path()]) -> wtb_route().
new(Name, Paths) ->
    #wtb_route{name=Name, path=Paths}.

-spec new(wtb_route_name(), [wtb_route_path()], [wtb_method()]) ->
                   wtb_route().
new(Name, Paths, Methods) ->
    #wtb_route{name=Name, path=Paths, methods=Methods}.

-spec handler(wtb_route_name(), [{atom(), function()}], 
              [wtb_route_path()], [wtb_method()]) -> wtb_route().
handler(Name, Funs, Paths, Methods) ->
    #wtb_route{name=Name, path=Paths, methods=Methods, handlers=Funs}.

-spec static([wtb_route_path()], string()) -> 
                    wtb_route().
static(Paths, StaticRoot) ->
    static(Paths, StaticRoot, "index.html").

-spec static([wtb_route_path()], string(), 
             string()) -> wtb_route().
static(Paths, StaticRoot, DefaultFile) ->
    Options = [{static_root, StaticRoot},
               {default_file, DefaultFile}],
    #wtb_route{name=static, 
               path=Paths, 
               provides=[requested],
               resource=webturbine_static_res,
               options=Options}.

-spec find_from_path([wtb_route()], [string()], [{atom(), string()}]) -> 
                       wtb_route() | {error, not_found}.
find_from_path([Route|Rest], PathTokens, PathInfo) ->
    case matches_path(Route, PathTokens, PathInfo) of
        {true, R} -> R;
        _ -> find_from_path(Rest, PathTokens, PathInfo)
    end;
find_from_path([], _, _) ->
    {error, not_found}.

-spec get_field(atom(), wtb_route()) -> term().
get_field(Field, Route) -> 
    case field_num(Field) of
        {error, not_found} ->
            {error, not_found};
        Ind ->
            element(Ind, Route)
    end.

-spec set_field(atom(), term(), wtb_route()) -> wtb_route().
set_field(Field, Value, Route) -> 
    case field_num(Field) of
        {error, not_found} ->
            {error, not_found};
        Ind ->
            setelement(Ind, Route, Value)
    end.

-spec matches_path(wtb_route(), [string()], [{atom(), string()}]) -> 
                          boolean() | {true, wtb_route()}.
matches_path(Route, PathTokens, PathInfo) ->
    RoutePaths = generate_paths(Route),
    paths_match_path(RoutePaths, PathTokens, PathInfo).

-spec generate_flat_paths(wtb_route()) -> [wtb_route_path()].
generate_flat_paths(Route) ->
    lists:append([ Paths || {_, Paths} <- generate_paths(Route) ]).

-spec generate_paths(wtb_route()) -> [{wtb_route(), [wtb_route_path()]}].
generate_paths(Route=#wtb_route{routes=[]}) ->
    [{Route, generate_sub_paths(Route)}];
generate_paths(Route=#wtb_route{routes=SubRoutes}) ->
    TopRoutePaths = generate_sub_paths(Route),
    SubRoutePaths = 
        lists:flatten([generate_paths(
                         append_prefixes(TopRoutePaths, SubRoute))
                       || SubRoute <- SubRoutes ]),
    [{Route, TopRoutePaths} | SubRoutePaths].

%%====================================================================
%% Internal functions
%%====================================================================

-spec generate_sub_paths(wtb_route()) -> [wtb_route_path()].
generate_sub_paths(#wtb_route{prefix=[], path=Paths}) ->
    Paths;
generate_sub_paths(#wtb_route{prefix=Prefixes, path=[]}) ->
    Prefixes;
generate_sub_paths(#wtb_route{prefix=Prefixes, path=Paths}) ->
    PrefixGroups = [ [ Prefix ++ Path || Path <- Paths ] || Prefix <- Prefixes ],
    lists:append(PrefixGroups).

-spec append_prefixes([wtb_route_path()], wtb_route()) -> wtb_route().
append_prefixes(NewPrefixes, Route) ->
    Prefixes = get_field(prefix, Route),
    Prefixes1 = NewPrefixes ++ Prefixes,
    set_field(prefix, Prefixes1, Route).

-spec paths_match_path([[wtb_route_path()] | {wtb_route(), [wtb_route_path()]}], 
                       [string()], [{atom(), string()}]) -> 
                              boolean() | {true, wtb_route()}.
paths_match_path([], _, _) ->
    false;
paths_match_path([{Route, RoutePaths}|Rest], PathTokens, PathInfo) ->
    case paths_match_path(RoutePaths, PathTokens, PathInfo) of
        true ->
            {true, Route};
        false ->
            paths_match_path(Rest, PathTokens, PathInfo)
    end;
paths_match_path([RoutePath|Rest], PathTokens, PathInfo) ->
    case parts_match_path(RoutePath, PathTokens, PathInfo) of
        true ->
            true;
        false -> 
            paths_match_path(Rest, PathTokens, PathInfo)
    end.

-spec parts_match_path(wtb_route_path(), [string()], [{atom(), string()}]) -> boolean().
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

-spec field_num(atom()) -> {error, not_found} | non_neg_integer().
field_num(Field) ->
  Fields = record_info(fields, wtb_route),
  DifField = fun (FieldName) -> Field /= FieldName end,
  case length(lists:takewhile(DifField, Fields)) of
    Length when Length =:= length(Fields) ->
      {error, not_found};
    Length ->
      Length + 2
  end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_paths_test() ->
    R1 = new(one),
    R2 = new(two, [["one", "two"]]),
    R3 = #wtb_route{
                      prefix=[["one"], ["two"]], 
                      path=[["three"],["four"]]},
    ?assertEqual([{R1, [["one"]]}], generate_paths(R1)),
    ?assertEqual([{R2, [["one", "two"]]}], generate_paths(R2)),
    ?assertEqual([{R3, [["one", "three"],
                        ["one", "four"],
                        ["two", "three"],
                        ["two", "four"]]}],
                 generate_paths(R3)),
    
    Inner1 = new(inner1),
    Inner2 = new(inner2),
    Outer = #wtb_route{name=outer,
                       routes=[Inner1,Inner2],
                       prefix=[["outer1"],["outer2"]],
                       path=[["middle1"],["middle2"]]},
    OuterPaths = [["outer1","middle1"],
                  ["outer1","middle2"],
                  ["outer2","middle1"],
                  ["outer2","middle2"]],
    ?assertEqual([{Outer, OuterPaths},
                  {set_field(prefix, OuterPaths, Inner1),
                   [["outer1","middle1","inner1"],
                    ["outer1","middle2","inner1"],
                    ["outer2","middle1","inner1"],
                    ["outer2","middle2","inner1"]]},
                  {set_field(prefix, OuterPaths, Inner2),
                   [["outer1","middle1","inner2"],
                    ["outer1","middle2","inner2"],
                    ["outer2","middle1","inner2"],
                    ["outer2","middle2","inner2"]]}],
                 generate_paths(Outer)).

path_match_test() ->
    ?assertEqual(true, parts_match_path(["hello"],["hello"],[])),
    ?assertEqual(true, parts_match_path(["hello", one],["hello", "one"],[{one, "one"}])),
    ?assertEqual(true, parts_match_path(["hello", '*'],["hello", "one", "two", "three"],[])),
    ?assertEqual(false, parts_match_path(["not", '*'],["hello", "one", "two", "three"],[])).

from_path_test() ->
    R1 = new(hello),
    R2 = new(node, [["nodes", node]]),
    R3 = #wtb_route{name = cluster,
                    path = [["clusters", cluster]],
                    routes = [R2]},
    Routes = [R1, R2, R3],
    ?assertEqual(
       R1, find_from_path(Routes,["hello"],[])),
    
    ?assertEqual(
       wtb_route:set_field(prefix, [["clusters", cluster]], R2),
       find_from_path(
         Routes,
         string:tokens("clusters/cluster/nodes/hello", "/"),
         [{cluster, "cluster"},{node,"hello"}])).

-endif.
