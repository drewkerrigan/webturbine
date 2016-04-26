# Webturbine

An Erlang Webmachine resource library for faster and less verbose API development.

## Rebar Configuration

In your rebar.config, ensure the following is there:

```
{deps, [
    {webturbine, ".*", {git, "git://github.com/drewkerrigan/webturbine", {branch, "master"}}}
]}.
```

## Webmachine Integration

Webturbine relies on webmachine, so you'll still need to start Webmachine normally. The resources you intend to serve requests with need to be passed into the dispatch/1. Here's an example supervisor spec:

```
MyResources = [myresource],

DispatchList = webturbine_route:dispatch(MyResources),

WebConfig = [
    {ip, "127.0.0.1"},
    {port, 9000},
    {dispatch, DispatchList}
],

WebSpec = {webmachine_mochiweb,
      {webmachine_mochiweb, start, [WebConfig]},
      permanent, 5000, worker, [mochiweb_socket_server]},
      
{ok, { {one_for_one, 10, 10}, [WebSpec]} }.
```

## Resource Definition

### Behaviour

Your resource module should implement the `webturbine_resource` behaviour like so:

```
-module(myresource).
-behaviour(webturbine_resource).
-export([something_get/0]).

%% Behaviour Callbacks:
routes() -> [wtb:route(something)].

%% Route Callbacks:
something_get() -> "You got something!".
```

### Routes

Routes can be specified as records, or as function calls to `wtb:route/1-7`. Here is an example route which uses all of the available fields:

```
routes() ->
    [
     #wtb_route{
         name = myroute,
         path = [["my", "route", var],["my", "alias", var]],
         methods = ['GET'],
         provides = [json],
         accepts = [any],
         routes = [
             #wtb_route{
                 name = myroute2,
                 path = [["part", "two", var2]],
                 provides = [json]
         ],
         prefix = ["this", "is"]}
    ].
```

That single `#wtb_route{}` definition will result in the following webmachine routes getting created:

```
GET /this/is/my/route/#{var}
GET /this/is/my/route/#{var}/part/two/#{var2}
GET /this/is/my/alias/#{var}
GET /this/is/my/alias/#{var}/part/two/#{var2}
```

Here is the same route definition using the shorthand `wtb:route/7`:

```
routes() ->
    [
     route(myroute, [["my", "route", var],["my", "alias", var]], 
           ['GET'], [json], [any], [
               route(myroute2, [["part", "two", var2]], [json])
           ], ["this", "is"])
    ].
```

### Callbacks

For routes that don't require request data, use this callback format:

```
RouteName_CallbackName() -> wtb_resp().
```

For routes that do require request data, use this callback format:

```
RouteName_CallbackName(wtb_req()) -> wtb_resp().
```

For routes that require a resource defined state, use this callback format:

```
RouteName_CallbackName(wtb_req(), term()) -> {wtb_resp(), term()}.
```

#### Available Callbacks Per Route:

* `init`
* `available`
* `get`
* `put`
* `post_path`
* `post`
* `delete`
* `last_modified`

### Example Resource

`cluster_manager_res.erl`:

```
-module(cluster_manager_res).
-behaviour(webturbine_resource).
-export([cluster_exists/1,
         cluster_get/1,
         node_exists/1,
         node_get/1]).

routes() -> 
    [
     wtb:route(cluster, [["clusters", cluster]], ['GET'], [json], [any], [
         wtb:route(node, [["nodes", node]], ['GET'], [json])
     ])
    ].

%% Cluster
cluster_exists(Req) -> 
    ClusterKey = wrq:path_info(cluster, Req),
    % Some logic with ClusterKey
    true.
cluster_get(Req) ->
    ClusterKey = list_to_binary(wrq:path_info(cluster, Req)),
    [{ClusterKey, [
        {nodes, [node1, node2, node3]}
    ]}].
    
%% Node
node_exists(Req) -> 
    NodeKey = wrq:path_info(node, Req),
    case cluster_exists(Req) of
        true ->
            %% Some logic with NodeKey
            true;
        false ->
            false
    end.
node_get(Req) ->
    ClusterKey = list_to_binary(wrq:path_info(cluster, Req)),
    NodeKey = list_to_binary(wrq:path_info(node, Req)),
    [{NodeKey, [
        {cluster, ClusterKey},
        {host, <<"localhost:8098">>}]
    }].
```
