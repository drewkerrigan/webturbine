-module(wtb).

-export([route/7, route/6, route/5, route/4, route/3, route/2, route/1]).

-include("webturbine.hrl").

-spec route(wtb_route_name()) -> wtb_route().
route(Name) ->
    route(Name, [[atom_to_list(Name)]], ['GET'], [any], [any], [], []).

-spec route(wtb_route_name(), [wtb_route_path()]) -> wtb_route().
route(Name, Paths) ->
    route(Name, Paths, ['GET'], [any], [any], [], []).

-spec route(wtb_route_name(), [wtb_route_path()], [wtb_method()]) ->
                   wtb_route().
route(Name, Paths, Methods) ->
    route(Name, Paths, Methods, [any], [any], [], []).

-spec route(wtb_route_name(), [wtb_route_path()], [wtb_method()], 
            [wtb_type()]) -> wtb_route().
route(Name, Paths, Methods, Provides) ->
    route(Name, Paths, Methods, Provides, [any], [], []).

-spec route(wtb_route_name(), [wtb_route_path()], [wtb_method()], 
            [wtb_type()], [wtb_type()]) -> wtb_route().
route(Name, Paths, Methods, Provides, Accepts) ->
    route(Name, Paths, Methods, Provides, Accepts, [], []).

-spec route(wtb_route_name(), [wtb_route_path()], [wtb_method()], 
            [wtb_type()], [wtb_type()], [wtb_route()]) -> wtb_route().
route(Name, Paths, Methods, Provides, Accepts, Routes) ->
    route(Name, Paths, Methods, Provides, Accepts, Routes, []).

-spec route(wtb_route_name(), [wtb_route_path()], [wtb_method()], 
            [wtb_type()], [wtb_type()], [wtb_route()], [wtb_route_path()]) ->
                   wtb_route().
route(Name, Paths, Methods, Provides, Accepts, Routes, Prefixes) ->
    #wtb_route{name = Name,
               path = Paths,
               methods = Methods,
               provides = Provides,
               accepts = Accepts,
               routes = Routes,
               prefix = Prefixes}.
