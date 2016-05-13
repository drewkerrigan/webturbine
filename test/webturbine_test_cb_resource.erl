-module(webturbine_test_cb_resource).

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
                prefix = [["base"]],
                path = [["echo", this]],
                provides = [text]},
     #wtb_route{name = cluster,
                path = [["clusters", cluster]],
                routes = [
                          wtb_route:new(node, [["nodes", node]])
                         ]},
     wtb_route:new(short),
     %% wtb_route:static([["static",'*']], "./"),
     wtb_route:handler(handler, [{handler_get, fun(_,S)-> {"handler_value",S} end}], [["handler"]], ['GET'])
    ].

echo_get(Req) ->
    cowboy_req:binding(this, Req, {error, not_found}).

cluster_init() ->
    #state{}.
cluster_exists(Req, State) ->
    case cowboy_req:binding(cluster, Req) of
        <<"here">> ->
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
    "Cluster: " ++ binary_to_list(cowboy_req:binding(cluster, Req)) ++ ", Node: " ++ binary_to_list(cowboy_req:binding(node, Req)).

short_get() ->
    [{its, <<"json">>}].
