-module(webturbine_static_res).

-export([static_exists/2,
         static_last_modified/2,
         static_get/2]).

-include_lib("kernel/include/file.hrl").

%%%===================================================================
%%% Callbacks
%%%===================================================================

static_exists(ReqData, Options) ->
    Filename = static_filename(ReqData, Options),
    {filelib:is_regular(Filename), Options}.

static_last_modified(ReqData, Options) ->
    LM = filelib:last_modified(static_filename(ReqData, Options)),
    {LM, Options}.

static_get(ReqData, Options) ->
    Filename = static_filename(ReqData, Options),
    {ok, Response} = file:read_file(Filename),
    {Response, Options}.

%% ====================================================================
%% Private
%% ====================================================================

static_filename(ReqData, Options) ->
    StaticRoot = proplists:get_value(static_root, Options, "priv/www"),
    DefaultFile = proplists:get_value(static_root, Options, "index.html"),

    case wtb_reqdata:disp_path(ReqData) of
        "" ->
            filename:join([StaticRoot, DefaultFile]);
        F ->
            filename:join([StaticRoot, F])
    end.
