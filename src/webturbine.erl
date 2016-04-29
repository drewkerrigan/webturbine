-module(webturbine).

-export([dispatch/1]).

-include("webturbine.hrl").

%%====================================================================
%% API
%%====================================================================

-spec dispatch([module()]) -> [{[string() | atom], module(), [term()]}].
dispatch(Resources) ->
    WMRoutes = 
        [ [ [ {Path, webturbine_wm_resource, [R]} 
              || Path <- wtb_route:generate_flat_paths(Route) ]
            || Route <- Routes ]
          || {R, Routes} <- [ {R, R:routes()} || R <- Resources ] ],
    lists:flatten(WMRoutes).
