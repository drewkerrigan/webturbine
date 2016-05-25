-module(wtb_route).

-export([new/1, new/2, new/3,
         handler/4,
         static/2, static/3,
         get_field/2,
         set_field/3,
         generate_paths/2]).

-export([init/1,
         call_exists/2,
         call_noop/3,
         call_bool/3, 
         call_content/3,
         call_websocket/4,
         call/3, call/4,
         generate_callback/3,
         to_noop_response/1,
         to_bool_response/1,
         to_content_response/1,
         to_websocket_response/1]).

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
               state=Options}.

-spec get_field(atom(), wtb_route()) -> term().
get_field(Field, Route) -> 
    case field_num(Field) of
        {error, not_found} ->
            {error, not_found};
        Ind ->
            element(Ind, Route)
    end.

-spec set_field(atom(), term(), wtb_route()) -> {error, not_found} | wtb_route().
set_field(Field, Value, Route) -> 
    case field_num(Field) of
        {error, not_found} ->
            {error, not_found};
        Ind ->
            setelement(Ind, Route, Value)
    end.

-spec generate_paths(module(), wtb_route()) -> [{wtb_route(), [wtb_route_path()]}].
generate_paths(Resource, Route=#wtb_route{routes=[]}) ->
    generate_sub_paths(maybe_set_resource(Resource, Route));
generate_paths(Resource, Route=#wtb_route{routes=SubRoutes}) ->
    Route1 = maybe_set_resource(Resource, Route),
    TopRoutePaths = generate_sub_paths(Route1),
    SubRoutePaths = 
        lists:flatten([generate_paths(
                         Resource,
                         append_prefixes(TopRoutePaths, 
                                         maybe_set_resource(Resource, SubRoute)))
                       || SubRoute <- SubRoutes ]),
    lists:append(TopRoutePaths, SubRoutePaths).

%%====================================================================
%% Callbacks
%%====================================================================

init(Route=#wtb_route{request=ReqData, state=State}) ->
    Callback = generate_callback(init, Route, [ReqData, State]),
    {State1, _, Route1} = call(Callback, Route, State),
    Route1#wtb_route{state=State1}.

call_exists(Name, #wtb_route{resource=Resource}) ->
    case find_callback(Resource, Name, 2) of
        {error, callback_not_implemented} -> false;
        _ -> true
    end.

call_noop(Name, Route=#wtb_route{request=ReqData, 
                                 state=State}, Default) ->
    Callback = generate_callback(Name, Route, [ReqData, State]),
    call(Callback, to_noop_response, Route, Default).

call_bool(Name, Route=#wtb_route{request=ReqData, 
                                 state=State}, Default) ->
    Callback = generate_callback(Name, Route, [ReqData, State]),
    call(Callback, to_bool_response, Route, Default).

call_content(Name, Route=#wtb_route{request=ReqData, 
                                    state=State}, Default) ->
    Callback = generate_callback(Name, Route, [ReqData, State]),
    call(Callback, to_content_response, Route, Default).

call_websocket(Name, Message, Route=#wtb_route{request=ReqData, 
                                               state=State}, Default) ->
    Callback = generate_callback(Name, Route, [Message, ReqData, State]),
    call(Callback, to_websocket_response, Route, Default).

generate_callback(Name, #wtb_route{resource=Resource, 
                                   name=RouteName,
                                   handlers=undefined}, Args) ->
    Callback = list_to_atom(atom_to_list(RouteName) ++ "_" ++ atom_to_list(Name)),
    case find_callback(Resource, Callback, length(Args)) of
        {R,C,N} -> 
            {R, C, lists:sublist(Args, N)};
        {error, callback_not_implemented} ->
            {error, callback_not_implemented}
    end;
generate_callback(Name, #wtb_route{name=RouteName,
                                   handlers=Handlers}, Args) ->
    Callback = list_to_atom(atom_to_list(RouteName) ++ "_" ++ atom_to_list(Name)),
    case proplists:get_value(Callback, Handlers) of
        undefined ->
            {error, callback_not_implemented};
        Fun ->
            {Fun, Args}
    end.

call(MFA, Route, Default) ->
    call(MFA, to_noop_response, Route, Default).

call({error, callback_not_implemented}, _, Route=#wtb_route{request=ReqData}, Default) ->
    {Default, ReqData, Route};
call(MFA, Modifier, Route=#wtb_route{request=ReqData}, _) ->
    ResultAndLength = 
        case MFA of
            {M, F, A} ->
                {erlang:apply(M, F, A), length(A)};
            {F, A} ->
                {erlang:apply(F, A), length(A)}
        end,
    Result = 
        case ResultAndLength of
            {{R, State1}, N} when N > 1 -> 
                Route1 = Route#wtb_route{state=State1},
                {R, ReqData, Route1};
            {R, _} ->
                {R, ReqData, Route}
        end,
    erlang:apply(?MODULE, Modifier, [Result]).

to_noop_response(R) -> R.

to_bool_response({Response, ReqData, Ctx}) when is_boolean(Response) ->
    {Response, ReqData, Ctx};
to_bool_response({{error, not_found}, ReqData, Ctx}) ->
    {false, ReqData, Ctx};
to_bool_response({{_,_}=Response, ReqData, Ctx}) ->
    to_content_response({Response, ReqData, Ctx});
to_bool_response({Content, ReqData, Ctx}) ->
    Content1 = to_content(Content),
    ReqData1 = wtb_reqdata:append_to_response_body(Content1, ReqData),
    {true, ReqData1, Ctx}.

to_content_response({Content, ReqData, Ctx}) when is_atom(Content) ->
    {atom_to_list(Content), ReqData, Ctx};
to_content_response({{halt, Code}, ReqData, Ctx}) ->
    wtb_reqdata:halt(Code, ReqData, Ctx);
to_content_response({{{halt, Code}, Content}, ReqData, Ctx}) ->
    Content1 = to_content(Content),
    wtb_reqdata:halt(Code, Content1, ReqData, Ctx);
to_content_response({{error, not_found}, ReqData, Ctx}) ->
    wtb_reqdata:halt(404, ReqData, Ctx);
to_content_response({{error, Reason}, ReqData, Ctx}) ->
    Content = to_content([{error, list_to_binary(io_lib:format("~p", [Reason]))}]),
    wtb_reqdata:halt(500, Content, ReqData, Ctx);
to_content_response({Content, ReqData, Ctx}) ->
    Content1 = to_content(Content),
    {Content1, ReqData, Ctx}.

to_websocket_response({{text,_}=Response, ReqData, Ctx}) ->
    {reply, Response, ReqData, Ctx, hibernate};
to_websocket_response({ok, ReqData, Ctx}) ->
    {ok, ReqData, Ctx, hibernate}.

to_content(Content) when is_binary(Content) ->
    Content;
to_content([{_,_}|_]=Content) ->
    mochijson2:encode(Content);
to_content(Content) when is_list(Content) ->
    Content;
to_content(Content) ->
    list_to_binary(io_lib:format("~p", [Content])).

%%====================================================================
%% Internal functions
%%====================================================================

maybe_set_resource(Resource, Route=#wtb_route{resource=undefined}) ->
    Route#wtb_route{resource=Resource};
maybe_set_resource(_, Route) -> 
    Route.

-spec generate_sub_paths(wtb_route()) -> [{wtb_route(), wtb_route_path()}].
generate_sub_paths(Route=#wtb_route{prefix=[], path=Paths}) ->
    [ {Route, Path} || Path <- Paths ];
generate_sub_paths(Route=#wtb_route{prefix=Prefixes, path=[]}) ->
    [ {Route, Path} || Path <- Prefixes ];
generate_sub_paths(Route=#wtb_route{prefix=Prefixes, path=Paths}) ->
    PrefixGroups = [ [ 
                       case Prefix of
                           {_R1, Path1} ->
                               Path1 ++ Path;
                           Path1 ->
                               Path1 ++ Path
                       end
                       || Path <- Paths ] 
                     || Prefix <- Prefixes ],
    [ {Route, Path} || Path <- lists:append(PrefixGroups) ].

-spec append_prefixes([wtb_route_path()], wtb_route()) -> wtb_route().
append_prefixes(NewPrefixes, Route) ->
    Prefixes = get_field(prefix, Route),
    Prefixes1 = NewPrefixes ++ Prefixes,
    set_field(prefix, Prefixes1, Route).

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

find_callback(_Resource, _Callback, -1) ->
    {error, callback_not_implemented};
find_callback(Resource, Callback, MaxArity) ->
    case erlang:function_exported(Resource, Callback, MaxArity) of
        true ->
            {Resource, Callback, MaxArity};
        false ->
            find_callback(Resource, Callback, MaxArity - 1)
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
    ?assertEqual([{R1, ["one"]}], generate_paths(undefined, R1)),
    ?assertEqual([{R2, ["one", "two"]}], generate_paths(undefined, R2)),
    ?assertEqual([{R3, ["one", "three"]},
                  {R3, ["one", "four"]},
                  {R3, ["two", "three"]},
                  {R3, ["two", "four"]}],
                 generate_paths(undefined, R3)),
    
    Inner1 = new(inner1),
    Inner2 = new(inner2),
    Outer = #wtb_route{name=outer,
                       routes=[Inner1,Inner2],
                       prefix=[["outer1"],["outer2"]],
                       path=[["middle1"],["middle2"]]},
    OuterPaths = [{Outer, ["outer1","middle1"]},
                  {Outer, ["outer1","middle2"]},
                  {Outer, ["outer2","middle1"]},
                  {Outer, ["outer2","middle2"]}],
    ModInner1 = set_field(prefix, OuterPaths, Inner1),
    ModInner2 = set_field(prefix, OuterPaths, Inner2),
    ?assertEqual(OuterPaths ++ 
                     [{ModInner1, ["outer1","middle1","inner1"]},
                      {ModInner1, ["outer1","middle2","inner1"]},
                      {ModInner1, ["outer2","middle1","inner1"]},
                      {ModInner1, ["outer2","middle2","inner1"]},
                      {ModInner2, ["outer1","middle1","inner2"]},
                      {ModInner2, ["outer1","middle2","inner2"]},
                      {ModInner2, ["outer2","middle1","inner2"]},
                      {ModInner2, ["outer2","middle2","inner2"]}],
                 generate_paths(undefined, Outer)).

-endif.
