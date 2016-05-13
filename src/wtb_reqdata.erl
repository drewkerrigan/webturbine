-module(wtb_reqdata).

-export([append_to_response_body/2,
         path/1,
         method/1,
         halt/3, halt/4,
         to_bool_response/1,
         to_content_response/1,
         to_content_types/3,
         to_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

append_to_response_body(Content, ReqData=#wm_reqdata{}) -> 
    wrq:append_to_response_body(Content, ReqData);
append_to_response_body(Content, ReqData) -> 
    cowboy_req:set_resp_body(Content, ReqData).

path(ReqData=#wm_reqdata{}) -> 
    wrq:path(ReqData);
path(ReqData) -> 
    cowboy_req:path(ReqData).

method(ReqData=#wm_reqdata{}) -> 
    wrq:method(ReqData);
method(ReqData) -> 
    list_to_atom(binary_to_list(cowboy_req:method(ReqData))).

halt(Code, ReqData=#wm_reqdata{}, Ctx) ->
    {{halt, Code}, ReqData, Ctx};
halt(Code, ReqData, Ctx) ->
    {stop, cowboy_req:reply(Code, ReqData), Ctx}.

halt(Code, Content, ReqData=#wm_reqdata{}, Ctx) ->
    ReqData1 = append_to_response_body(Content, ReqData),
    halt(Code, ReqData1, Ctx).

to_bool_response({Response, ReqData, Ctx}) when is_boolean(Response) ->
    {Response, ReqData, Ctx};
to_bool_response({{error, not_found}, ReqData, Ctx}) ->
    {false, ReqData, Ctx};
to_bool_response({{_,_}=Response, ReqData, Ctx}) ->
    to_content_response({Response, ReqData, Ctx});
to_bool_response({Content, ReqData, Ctx}) ->
    Content1 = to_content(Content),
    ReqData1 = append_to_response_body(Content1, ReqData),
    {true, ReqData1, Ctx}.

to_content_response({Content, ReqData, Ctx}) when is_atom(Content) ->
    {atom_to_list(Content), ReqData, Ctx};
to_content_response({{halt, Code}, ReqData, Ctx}) ->
    halt(Code, ReqData, Ctx);
to_content_response({{{halt, Code}, Content}, ReqData, Ctx}) ->
    Content1 = to_content(Content),
    halt(Code, Content1, ReqData, Ctx);
to_content_response({{error, not_found}, ReqData, Ctx}) ->
    halt(404, ReqData, Ctx);
to_content_response({{error, Reason}, ReqData, Ctx}) ->
    Content = to_content([{error, list_to_binary(io_lib:format("~p", [Reason]))}]),
    halt(500, Content, ReqData, Ctx);
to_content_response({Content, ReqData, Ctx}) ->
    Content1 = to_content(Content),
    {Content1, ReqData, Ctx}.

to_content(Content) when is_binary(Content) ->
    Content;
to_content([{_,_}|_]=Content) ->
    mochijson2:encode(Content);
to_content(Content) when is_list(Content) ->
    Content;
to_content(Content) ->
    list_to_binary(io_lib:format("~p", [Content])).

to_content_types(TypeShortcuts, Function, ReqData=#wm_reqdata{}) ->
    lists:append([ ct(CT, Function, ReqData, string) || CT <- TypeShortcuts ]);
to_content_types(TypeShortcuts, Function, ReqData) ->
    lists:append([ ct(CT, Function, ReqData, binary) || CT <- TypeShortcuts ]).

to_methods(Methods, #wm_reqdata{}) ->
    Methods;
to_methods(Methods, _) ->
    [ list_to_binary(atom_to_list(M)) || M <- Methods ].

%%====================================================================
%% Internal functions
%%====================================================================

-define(JSON_TYPE, <<"application/json">>).
-define(TEXT_TYPE, <<"plain/text">>).
-define(HTML_TYPE, <<"text/html">>).
-define(OCTET_TYPE, <<"application/octet-stream">>).
-define(FORM_TYPE, <<"application/x-www-form-urlencoded">>).

ct(Shortcut, Function, ReqData, string) ->
    CTs = ct(Shortcut, Function, ReqData),
    [ {binary_to_list(CT), F} || {CT, F} <- CTs ];
ct(Shortcut, Function, ReqData, binary) ->
    ct(Shortcut, Function, ReqData).

ct(json, F, _R) -> [{?JSON_TYPE, F}];
ct(binary, F, _R) -> [{?OCTET_TYPE, F}];
ct(text, F, _R) -> [{?TEXT_TYPE, F}];
ct(html, F, _R) -> [{?HTML_TYPE, F}];
ct(requested, F, R) -> 
    Type = webmachine_util:guess_mime(path(R)),
    [{list_to_binary(Type), F}];
ct(none, _F, _R) -> [];
ct(any, F, _R) -> 
    [{?FORM_TYPE, F},
     {?OCTET_TYPE, F},
     {?TEXT_TYPE, F},
     {?JSON_TYPE, F}].
