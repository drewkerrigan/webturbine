-module(webturbine_router).

%% API exports
-export([dispatch/1]).

%% Webmachine exports
-export([init/1,
         service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         delete_resource/2,
         process_post/2,
         provide_content/2,
         accept_content/2,
         post_is_create/2,
         create_path/2,
         last_modified/2]).


-include("webturbine.hrl").

-record(ctx, {
          wtb_resource :: module(),
          wtb_route :: wtb_route()
         }).

%%====================================================================
%% API
%%====================================================================

-spec dispatch([module()]) -> [{[string() | atom], module(), [term()]}].
dispatch(Resources) ->
    WtbRoutes = [ {R, R:routes()} || R <- Resources ],
    WmRoutes = [ build_wm_routes(WtbR, []) || WtbR <- WtbRoutes ],
    lists:flatten(WmRoutes).

%%====================================================================
%% Webmachine Callbacks
%%====================================================================

init([Resource]) ->
    Ctx = #ctx{wtb_resource = Resource},
    {ok, Ctx}.

service_available(ReqData, Ctx=#ctx{wtb_resource = Resource}) ->
    case get_route(Resource:routes(), ReqData) of
        #wtb_route{}=R ->
            Ctx1 = Ctx#ctx{wtb_route = R},
            Response = try_callback(available, true, ReqData, Ctx1),
            bool_resp(Response, ReqData, Ctx1);
        _ ->
            {false, ReqData, Ctx}
    end.

allowed_methods(ReqData, Ctx = #ctx{wtb_route = Route}) ->
    {Route#wtb_route.methods, ReqData, Ctx}.

content_types_provided(ReqData, Ctx = #ctx{wtb_route = Route}) ->
    Provides = to_content_types(Route#wtb_route.provides, provide_content, ReqData, []),
    {Provides, ReqData, Ctx}.

content_types_accepted(ReqData, Ctx = #ctx{wtb_route = Route}) ->
    Accepts = to_content_types(Route#wtb_route.accepts, accept_content, ReqData, []),
    {Accepts, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    Response = try_callback(exists, true, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx).

delete_resource(ReqData, Ctx) ->
    Response = try_callback(delete, true, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx).

provide_content(ReqData, Ctx) ->
    Response = try_callback(get, {error, not_found}, ReqData, Ctx),
    handle_resp(Response, ReqData, Ctx).

accept_content(ReqData, Ctx) ->
    Response = try_callback(put, false, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx).

process_post(ReqData, Ctx) ->
    Response = try_callback(post, false, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx).

post_is_create(ReqData, Ctx = #ctx{wtb_resource = Resource, wtb_route = Route}) ->    
    RouteName = Route#wtb_route.name,
    Callback = list_to_atom(atom_to_list(RouteName) ++ "_post_path"),
    {erlang:function_exported(Resource, Callback, 1), ReqData, Ctx}.

create_path(ReqData, Ctx) ->
    Response = try_callback(post_path, undefined, ReqData, Ctx),
    {Response, ReqData, Ctx}.

last_modified(ReqData, Ctx) ->
    Response = try_callback(last_modified, undefined, ReqData, Ctx),
    {Response, ReqData, Ctx}.

%%====================================================================
%% Internal functions
%%====================================================================

get_route([], _ReqData) ->
    undefined;
get_route([Route=#wtb_route{prefix=[],path=Paths} | Rest], ReqData) ->
    case get_route_path([], Paths, Route, ReqData) of
        undefined ->
            get_route(Rest, ReqData);
        R -> R
    end;
get_route([Route=#wtb_route{prefix=Bases,path=[]} | Rest], ReqData) ->
    case get_route_path([], Bases, Route, ReqData) of
        undefined ->
            get_route(Rest, ReqData);
        R -> R
    end;
get_route([Route=#wtb_route{prefix=Bases,path=Paths} | Rest], ReqData) ->
    case get_route_base(Bases, Paths, Route, ReqData) of
        undefined ->
            get_route(Rest, ReqData);
        R -> R
    end.

get_route_base([], _, _, _) ->
    undefined;
get_route_base([Base|Rest], Paths, Route, ReqData) ->
    case get_route_path(Base, Paths, Route, ReqData) of
        undefined ->
            get_route_base(Rest, Paths, Route, ReqData);
        R -> R
    end.

get_route_path(_, [], _, _) ->
    undefined;
get_route_path(Base, [Path|Rest], Route, ReqData) ->
    ReqPath = string:tokens(wrq:path(ReqData), "/"),
    case expand_path(Base ++ Path, ReqData, []) of
        ReqPath ->
            Route;
        _ ->
            get_route_path(Base, Rest, Route, ReqData)
    end.

expand_path([], _ReqData, Acc) ->
    lists:reverse(Acc);
expand_path([Part|Rest], ReqData, Acc) when is_list(Part) ->
    expand_path(Rest, ReqData, [Part | Acc]);
expand_path(['*'|Rest], ReqData, Acc) ->
    Tokens = string:tokens(wrq:path(ReqData), "/"),
    case length(Acc) > length(Tokens) of
        true ->
            undefined;
        false ->
            expand_path(Rest, ReqData, lists:reverse(lists:nthtail(length(Acc), Tokens)) ++ Acc)
    end;
expand_path([Part|Rest], ReqData, Acc) when is_atom(Part) ->
    expand_path(Rest, ReqData, [wrq:path_info(Part, ReqData) | Acc]).

build_wm_routes({_R, []}, Acc) ->
    lists:reverse(lists:flatten(Acc));
build_wm_routes({R, [#wtb_route{prefix = [], path = Paths} | Rest]}, Acc) ->
    build_wm_routes({R, Rest}, [build_wm_route(R, [], Paths, []) | Acc]);
build_wm_routes({R, [#wtb_route{prefix = Bases, path = []} | Rest]}, Acc) ->
    build_wm_routes({R, Rest}, [build_wm_route(R, [], Bases, []) | Acc]);
build_wm_routes({R, [#wtb_route{prefix = Bases, path = Paths} | Rest]}, Acc) ->
    build_wm_routes({R, Rest}, [build_wm_routes(R, Bases, Paths, []) | Acc]).

build_wm_routes(_R, [], _, Acc) ->
    Acc;
build_wm_routes(R, [Base|Rest], Paths, Acc) ->
    build_wm_routes(R, Rest, Paths, [build_wm_route(R, Base, Paths, [])|Acc]).

build_wm_route(_, _, [], Acc) ->
    Acc;
build_wm_route(R, Base, [Path|Rest], Acc) ->
    build_wm_route(R, Base, Rest, [{Base ++ Path, ?MODULE, [R]}|Acc]).

try_callback(CbName, Default, ReqData, #ctx{wtb_resource = Resource,
                              wtb_route = Route}) ->
    RouteName = Route#wtb_route.name,
    Callback = list_to_atom(atom_to_list(RouteName) ++ "_" ++ atom_to_list(CbName)),
    case erlang:function_exported(Resource, Callback, 1) of
        true ->
            Resource:Callback(ReqData);
        false ->
            Default
    end.

bool_resp(Response, ReqData, Ctx) when is_boolean(Response) ->
    {Response, ReqData, Ctx};
bool_resp({error, not_found}, ReqData, Ctx) ->
    {false, ReqData, Ctx};
bool_resp({_,_}=Response, ReqData, Ctx) ->
    handle_resp(Response, ReqData, Ctx);
bool_resp(Content, ReqData, Ctx) ->
    Content1 = to_wm_content(Content),
    ReqData1 = wrq:append_to_response_body(Content1, ReqData),
    {true, ReqData1, Ctx}.

handle_resp(true, ReqData, Ctx) ->
    {"", ReqData, Ctx};
handle_resp(false, ReqData, Ctx) ->
    {{halt, 500}, ReqData, Ctx};
handle_resp({halt, Code}, ReqData, Ctx) ->
    {{halt, Code}, ReqData, Ctx};
handle_resp({{halt, Code}, Content}, ReqData, Ctx) ->
    Content1 = to_wm_content(Content),
    ReqData1 = wrq:append_to_response_body(Content1, ReqData),
    {{halt, Code}, ReqData1, Ctx};
handle_resp({error, not_found}, ReqData, Ctx) ->
    {{halt, 404}, ReqData, Ctx};
handle_resp({error, Reason}, ReqData, Ctx) ->
    Content = to_wm_content([{error, list_to_binary(io_lib:format("~p", [Reason]))}]),
    ReqData1 = wrq:append_to_response_body(Content, ReqData),
    {{halt, 500}, ReqData1, Ctx};
handle_resp(Content, ReqData, Ctx) ->
    Content1 = to_wm_content(Content),
    {Content1, ReqData, Ctx}.

to_wm_content(Content) when is_binary(Content) ->
    Content;
to_wm_content([{_,_}|_]=Content) ->
    mochijson2:encode(Content);
to_wm_content(Content) when is_list(Content) ->
    Content;
to_wm_content(Content) ->
    list_to_binary(io_lib:format("~p", [Content])).

to_content_types([], _, _, Accum) ->
    lists:reverse(Accum);
to_content_types([json|Rest], Function, ReqData, Accum) ->
    to_content_types(Rest, Function, ReqData, [{?JSON_TYPE, Function}|Accum]);
to_content_types([binary|Rest], Function, ReqData, Accum) ->
    to_content_types(Rest, Function, ReqData, [{?OCTET_TYPE, Function}|Accum]);
to_content_types([text|Rest], Function, ReqData, Accum) ->
    to_content_types(Rest, Function, ReqData, [{?TEXT_TYPE, Function}|Accum]);
to_content_types([html|Rest], Function, ReqData, Accum) ->
    to_content_types(Rest, Function, ReqData, [{?HTML_TYPE, Function}|Accum]);
to_content_types([requested|Rest], Function, ReqData, Accum) ->
    Type = webmachine_util:guess_mime(wrq:disp_path(ReqData)),
    to_content_types(Rest, Function, ReqData, [{Type, Function}|Accum]);
to_content_types([none|_Rest], _Function, _ReqData, _Accum) ->
    [];
to_content_types([any|Rest], Function, ReqData, Accum) ->
    to_content_types(Rest, Function, ReqData, [{?FORM_TYPE, Function},
                            {?OCTET_TYPE, Function},
                            {?TEXT_TYPE, Function},
                            {?JSON_TYPE, Function} | Accum]);
to_content_types([Type|Rest], Function, ReqData, Accum) ->
    to_content_types(Rest, Function, ReqData, [{Type, Function}|Accum]).
