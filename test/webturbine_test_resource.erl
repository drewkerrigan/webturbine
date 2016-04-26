-module(webturbine_test_resource).

-behaviour(webturbine_resource).

-export([routes/0]).

-export([echo_get/1,
         cluster_init/0,
         cluster_exists/2,
         cluster_get/2,
         node_exists/1,
         node_get/1,
         short_get/0]).

-include("webturbine.hrl").

-record(state, {response = ""}).

routes() ->
    [
     #wtb_route{name = echo, 
                path = [["echo", this]],
                provides = [text]},
     #wtb_route{name = cluster,
                path = [["clusters", cluster]],
                routes = [
                          #wtb_route{name = node,
                                     path = [["nodes", node]]}
                         ]},
     wtb:route(short)
    ].

echo_get(Req) ->
    wrq:path_info(this, Req).

cluster_init() ->
    #state{}.
cluster_exists(Req, State) ->
    case wrq:path_info(cluster, Req) of
        "here" ->
            {true, State#state{response = "here"}};
        _ ->
            {false, State}
    end.
cluster_get(_, State=#state{response=Response}) ->
    {Response, State}.

node_exists(Req) ->
    {ClusterExists, _} = cluster_exists(Req, #state{}),
    ClusterExists.
        
node_get(Req) ->
    "Cluster: " ++ wrq:path_info(cluster, Req) ++ ", Node: " ++ wrq:path_info(node, Req).

short_get() ->
    [{its, <<"json">>}].
