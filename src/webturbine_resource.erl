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

%% For routes that don't require request data:
%% -callback RouteName_CallbackName() -> 
%%     wtb_resp().

%% For routes that do require request data:
%% -callback RouteName_CallbackName(wtb_req()) -> 
%%     wtb_resp().

%% For routes that require a resource defined state:
%% -callback RouteName_CallbackName(wtb_req(), term()) -> 
%%     {wtb_resp(), term()}.

%% Available Callbacks Per Route:
%%   * init
%%   * available
%%   * get
%%   * put
%%   * post_path
%%   * post
%%   * delete
%%   * last_modified
