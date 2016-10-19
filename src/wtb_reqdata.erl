%% (guess_mime/1 derived from code copyright 2007-2008 Basho Technologies)

-module(wtb_reqdata).

-export([append_to_response_body/2,
         path/1,
         disp_path/1,
         path_info/2, path_info/3,
         method/1,
         halt/3, halt/4,
         to_content_types/3,
         to_methods/2]).

append_to_response_body(Content, ReqData) ->
    cowboy_req:set_resp_body(Content, ReqData).

path(ReqData) ->
    cowboy_req:path(ReqData).

disp_path(ReqData) ->
    path_info('*', ReqData).

path_info(Name, ReqData) ->
    path_info(Name, ReqData, undefined).

path_info(Name, ReqData, Default) ->
    cowboy_req:binding(Name, ReqData, Default).

method(ReqData) ->
    list_to_atom(binary_to_list(cowboy_req:method(ReqData))).

halt(Code, ReqData, Ctx) ->
    {stop, cowboy_req:reply(Code, ReqData), Ctx}.

halt(Code, Content, ReqData, Ctx) ->
    ReqData1 = append_to_response_body(Content, ReqData),
    halt(Code, ReqData1, Ctx).

to_content_types(TypeShortcuts, Function, ReqData) ->
    lists:append([ ct(CT, Function, ReqData, binary) || CT <- TypeShortcuts ]).

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

ct(Shortcut, Function, ReqData, binary) ->
    ct(Shortcut, Function, ReqData).

ct(json, F, _R) -> [{?JSON_TYPE, F}];
ct(binary, F, _R) -> [{?OCTET_TYPE, F}];
ct(text, F, _R) -> [{?TEXT_TYPE, F}];
ct(html, F, _R) -> [{?HTML_TYPE, F}];
ct(requested, F, R) ->
    Type = guess_mime(path(R)),
    [{list_to_binary(Type), F}];
ct(none, _F, _R) -> [];
ct(any, F, _R) ->
    [{?FORM_TYPE, F},
     {?OCTET_TYPE, F},
     {?TEXT_TYPE, F},
     {?JSON_TYPE, F}].

%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) ->
    case filename:extension(File) of
        ".bz2" ->
            "application/x-bzip2";
        ".css" ->
            "text/css";
        ".eot" ->
            "application/vnd.ms-fontobject";
        ".gif" ->
            "image/gif";
        ".gz" ->
            "application/x-gzip";
        ".htc" ->
            "text/x-component";
        ".html" ->
            "text/html";
        ".ico" ->
            "image/x-icon";
        ".jpeg" ->
            "image/jpeg";
        ".jpg" ->
            "image/jpeg";
        ".js" ->
            "application/x-javascript";
        ".less" ->
            "text/css";
        ".m4v" ->
            "video/mp4";
        ".manifest" ->
            "text/cache-manifest";
        ".mp4" ->
            "video/mp4";
        ".oga" ->
            "audio/ogg";
        ".ogg" ->
            "audio/ogg";
        ".ogv" ->
            "video/ogg";
        ".otf" ->
            "font/opentyp";
        ".png" ->
            "image/png";
        ".svg" ->
            "image/svg+xml";
        ".svgz" ->
            "image/svg+xml";
        ".swf" ->
            "application/x-shockwave-flash";
        ".tar" ->
            "application/x-tar";
        ".tgz" ->
            "application/x-gzip";
        ".ttc" ->
            "application/x-font-ttf";
        ".ttf" ->
            "application/x-font-ttf";
        ".vcf" ->
            "text/x-vcard";
        ".webm" ->
            "video/web";
        ".webp" ->
            "image/web";
        ".woff" ->
            "application/x-font-woff";
        ".xhtml" ->
            "application/xhtml+xml";
        ".xml" ->
            "application/xml";
        ".zip" ->
            "application/zip";
        _ ->
            "text/plain"
    end.
