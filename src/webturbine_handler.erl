-module(webturbine_handler).

%% Shared exports
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         delete_resource/2,
         last_modified/2,
         generate_etag/2]).
-export([provide_content/2,
         accept_content/2]).

%% Webmachine exports
-export([init/1]).
-export([process_post/2,
         post_is_create/2,
         create_path/2]).

%% Cowboy exports
-export([init/2]).
-export([allow_missing_post/2]).

-include("webturbine.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%====================================================================
%% Shared Callbacks
%%====================================================================

service_available(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    Route2 = wtb_route:init(Route1),
    wtb_route:call_bool(available, Route2, true).

allowed_methods(ReqData, Route=#wtb_route{methods=Methods}) ->
    Methods1 = wtb_reqdata:to_methods(Methods, ReqData),
    {Methods1, ReqData, wtb_route:set_field(request, ReqData, Route)}.

content_types_provided(ReqData, Route=#wtb_route{provides=Provides}) ->
    Provides1 = wtb_reqdata:to_content_types(Provides, provide_content, ReqData),
    {Provides1, ReqData, wtb_route:set_field(request, ReqData, Route)}.

content_types_accepted(ReqData, Route=#wtb_route{accepts=Accepts}) ->
    Accepts1 = wtb_reqdata:to_content_types(Accepts, accept_content, ReqData),
    {Accepts1, ReqData, wtb_route:set_field(request, ReqData, Route)}.

resource_exists(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(exists, Route1, true).

delete_resource(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(delete, Route1, false).

provide_content(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_content(get, Route1, {error, not_found}).

accept_content(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    case wtb_reqdata:method(ReqData) of
        'POST' ->
            wtb_route:call_bool(post, Route1, false);
        'PUT' ->
            wtb_route:call_bool(put, Route1, false)
    end.

last_modified(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call(last_modified, Route1, undefined).

generate_etag(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_content(etag, Route1, undefined).

%%====================================================================
%% Cowboy Callbacks
%%====================================================================

init(ReqData, [Route]) ->
    {cowboy_rest, ReqData, Route}.

allow_missing_post(ReqData, Route) -> 
    Route1 = wtb_route:set_field(request, ReqData, Route),
    case wtb_route:call(post_path, Route1, undefined) of
        {undefined, ReqData1, Route1} ->
            {false, ReqData1, Route1};
        {_, ReqData1, Route1} ->
            {true, ReqData1, Route1}
    end.

%%====================================================================
%% Webmachine Callbacks
%%====================================================================

init([Route]) ->
    {ok, Route}.

process_post(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(post, Route1, false).

post_is_create(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    {wtb_route:call_exists(post_path, Route), ReqData, Route1}.

create_path(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_content(post_path, Route1, undefined).
