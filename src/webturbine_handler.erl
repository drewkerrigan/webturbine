-module(webturbine_handler).

%% Shared exports
-export([init/2]).
-export([allow_missing_post/2,
         websocket_handle/3,
         websocket_info/3,
         terminate/3,
         service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         is_conflict/2,
         previously_existed/2,
         delete_resource/2,
         last_modified/2,
         generate_etag/2,
         malformed_request/2,
         provide_content/2,
         accept_content/2]).


-include("webturbine.hrl").

%%====================================================================
%% Cowboy Callbacks
%%====================================================================

init(ReqData, [Route=#wtb_route{handler_type=rest}]) ->
    {cowboy_rest, ReqData, Route};
init(ReqData, [Route=#wtb_route{handler_type=websocket}]) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    Route2 = wtb_route:init(Route1),
    {cowboy_websocket, ReqData, Route2}.

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

is_conflict(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(is_conflict, Route1, false).

previously_existed(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(previously_existed, Route1, false).

delete_resource(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(delete, Route1, false).

provide_content(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    Data = wtb_route:call_content(get, Route1, {error, not_found}),
    Data.

malformed_request(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_bool(malformed_request, Route1, false).

accept_content(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    case wtb_reqdata:method(ReqData) of
        'PATCH' ->
            wtb_route:call_bool(patch, Route1, false);
        'POST' ->
            wtb_route:call_bool(post, Route1, false);
        'PUT' ->
            wtb_route:call_bool(put, Route1, false)
    end.

allow_missing_post(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    case wtb_route:call_noop(post_path, Route1, undefined) of
        {undefined, ReqData1, Route1} ->
            {false, ReqData1, Route1};
        {_, ReqData1, Route1} ->
            {true, ReqData1, Route1}
    end.

last_modified(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_noop(last_modified, Route1, undefined).

generate_etag(ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_noop(etag, Route1, undefined).

terminate(Reason, ReqData, Route=#wtb_route{handler_type=websocket}) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    {Resp, _, _, _, _} = wtb_route:call_websocket(terminate, Reason, Route1, ok),
    Resp;
terminate(Reason, ReqData, Route=#wtb_route{handler_type=rest}) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    {Resp, _, _, _, _} = wtb_route:call_content(terminate, Route1, Reason),
    Resp.

websocket_handle(Message, ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_websocket(handle, Message, Route1, ok).

websocket_info(Message, ReqData, Route) ->
    Route1 = wtb_route:set_field(request, ReqData, Route),
    wtb_route:call_websocket(info, Message, Route1, ok).
