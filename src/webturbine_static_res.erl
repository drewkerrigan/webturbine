-module(webturbine_static_res).

-export([static_exists/2,
         static_last_modified/2,
         static_get/2,
         static_etag/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%%===================================================================
%%% Callbacks
%%%===================================================================

static_exists(ReqData, Options) ->
    Filename = static_filename(ReqData, Options),
    io:format("list: ~p", [file:list_dir(proplists:get_value(static_root, Options))]),
    {filelib:is_regular(Filename), Options}.

static_last_modified(ReqData, Options) ->
    LM = filelib:last_modified(static_filename(ReqData, Options)),
    {LM, Options}.

static_get(ReqData, Options) ->
    Filename = static_filename(ReqData, Options),
    {ok, Response} = file:read_file(Filename),
    ET = hash_body(Response),
    Options1 = [{etag, webmachine_util:quoted_string(ET)}|Options],
    {Response, Options1}.

static_etag(_, Options) ->
    {proplists:get_value(etag, Options), Options}.

%% ====================================================================
%% Private
%% ====================================================================

static_filename(ReqData, Options) ->
    StaticRoot = proplists:get_value(static_root, Options, "priv/www"),
    DefaultFile = proplists:get_value(static_root, Options, "index.html"),
    case wrq:disp_path(ReqData) of
        "" ->
            filename:join([StaticRoot, DefaultFile]);
        F ->
            filename:join([StaticRoot, F])
    end.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:hash(sha,Body))).
