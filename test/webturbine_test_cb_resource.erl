-module(webturbine_test_cb_resource).

-behaviour(webturbine_resource).

-export([routes/0]).

-export([echo_get/1,
         cluster_init/0,
         cluster_exists/2,
         cluster_get/2,
         node_exists/1,
         node_get/1,
         short_get/0,
         websocket_init/0,
         websocket_handle/1,
         websocket_info/1]).

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
     wtb_route:handler(handler, [{handler_get, fun(_,S)-> {"handler_value",S} end}], [["handler"]], ['GET']),
     #wtb_route{name = websocket,
                path = [["websocket"]],
                handler_type = websocket}
    ].

echo_get(Req) ->
    wtb_reqdata:path_info(this, Req, {error, not_found}).

cluster_init() ->
    #state{}.
cluster_exists(Req, State) ->
    case wtb_reqdata:path_info(cluster, Req) of
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
    "Cluster: " ++ binary_to_list(wtb_reqdata:path_info(cluster, Req)) ++ ", Node: " ++ binary_to_list(wtb_reqdata:path_info(node, Req)).

short_get() ->
    [{its, <<"json">>}].

websocket_init() ->
    erlang:start_timer(1000, self(), <<"Hello!">>).

websocket_handle({text, Msg}) ->
    {text, <<"Very fine, thank you! ", Msg/binary>>};
websocket_handle(_) ->
    ok.

websocket_info({timeout, _Ref, Msg}) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{text, Msg};
websocket_info(_) ->
	ok.
