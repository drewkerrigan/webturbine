-module(webturbine_resource).

%% API exports
-export([]).

%%====================================================================
%% Behaviour Callbacks
%%====================================================================

-callback routes() -> 
    [wtb_route()].

-include("webturbine.hrl").

%%====================================================================
%% Optional Route Behaviour Callbacks
%%====================================================================

%% Format: RouteName_CallbackName(wtb_req()) | CallbackName(wtb_req()) -> 
%%     wtb_resp().

%% -callback RouteName_available(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_exists(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_get(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_put(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_post_path(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_post(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_delete(wtb_req())->
%%     wtb_resp().

%% -callback RouteName_last_modified(wtb_req()) ->
%%     wtb_resp().

%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================
