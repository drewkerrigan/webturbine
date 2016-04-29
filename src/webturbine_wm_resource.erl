-module(webturbine_wm_resource).

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
         last_modified/2,
         generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("webturbine.hrl").

-record(ctx, {
          wtb_resource :: module(),
          wtb_route :: wtb_route(),
          request_state :: any()
         }).

%%====================================================================
%% Webmachine Callbacks
%%====================================================================

init([Resource]) ->
    Ctx = #ctx{wtb_resource = Resource},
    {ok, Ctx}.

service_available(ReqData, Ctx=#ctx{wtb_resource = Resource}) ->
    Routes = 
        case find_callback(Resource, routes, 0) of
            {error, callback_not_implemented} ->
                [];
            _ ->
                Resource:routes()
        end,
    Tokens = string:tokens(wrq:path(ReqData), "/"),
    Info = wrq:path_info(ReqData),
    case wtb_route:find_from_path(Routes, Tokens, Info) of
        {error, not_found} ->
            {false, ReqData, Ctx};
        #wtb_route{}=Route ->
            Resource1 = 
                case Route#wtb_route.resource of
                    undefined ->
                        Resource;
                    Resource2 ->
                        Resource2
                end,
            ReqState = Route#wtb_route.options,
            Ctx1 = Ctx#ctx{wtb_route = Route,
                           wtb_resource = Resource1, 
                           request_state = ReqState},
            {ReqState1, Ctx2} = try_callback(init, ReqState, ReqData, Ctx1),
            Ctx3 = Ctx2#ctx{request_state = ReqState1},
            {Response, Ctx4} = try_callback(available, true, ReqData, Ctx3),
            bool_resp(Response, ReqData, Ctx4)
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
    {Response, Ctx1} = try_callback(exists, true, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx1).

delete_resource(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(delete, true, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx1).

provide_content(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(get, {error, not_found}, ReqData, Ctx),
    handle_resp(Response, ReqData, Ctx1).

accept_content(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(put, false, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx1).

process_post(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(post, false, ReqData, Ctx),
    bool_resp(Response, ReqData, Ctx1).

post_is_create(ReqData, Ctx = #ctx{wtb_resource = Resource, wtb_route = Route}) ->
    RouteName = Route#wtb_route.name,
    Callback = list_to_atom(atom_to_list(RouteName) ++ "_post_path"),
    case find_callback(Resource, Callback, 2) of
        {error, callback_not_implemented} ->
            {false, ReqData, Ctx};
        _ ->
            {true, ReqData, Ctx}
    end.

create_path(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(post_path, undefined, ReqData, Ctx),
    {Response, ReqData, Ctx1}.

last_modified(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(last_modified, undefined, ReqData, Ctx),
    {Response, ReqData, Ctx1}.

generate_etag(ReqData, Ctx) ->
    {Response, Ctx1} = try_callback(etag, undefined, ReqData, Ctx),
    {Response, ReqData, Ctx1}.

%%====================================================================
%% Internal functions
%%====================================================================

try_callback(CbName, Default, ReqData, Ctx=#ctx{wtb_resource = Resource,
                                                wtb_route = Route,
                                                request_state = State}) ->
    RouteName = Route#wtb_route.name,
    Callback = list_to_atom(atom_to_list(RouteName) ++ "_" ++ atom_to_list(CbName)),
    case find_callback(Resource, Callback, 2) of
        {R,C,0} -> 
            {R:C(), Ctx};
        {R,C,1} -> 
            {R:C(ReqData), Ctx};
        {R,C,2} -> 
            {Result, State1} = R:C(ReqData, State),
            Ctx1 = Ctx#ctx{request_state = State1},
            {Result, Ctx1};
        {error, callback_not_implemented} ->
            case Route#wtb_route.handlers of
                undefined ->
                    {Default, Ctx};
                Funs ->
                    case proplists:get_value(Callback, Funs) of
                        undefined ->
                            {Default, Ctx};
                        Fun ->
                            {Result, State1} = Fun(ReqData, State),
                            Ctx1 = Ctx#ctx{request_state = State1},
                            {Result, Ctx1}
                    end
            end
    end.

find_callback(_Resource, _Callback, -1) ->
    {error, callback_not_implemented};
find_callback(Resource, Callback, MaxArity) ->
    case erlang:function_exported(Resource, Callback, MaxArity) of
        true ->
            {Resource, Callback, MaxArity};
        false ->
            find_callback(Resource, Callback, MaxArity - 1)
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
