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
routes() -> [wtb_route:new(something)].

%% Route Callbacks:
something_get() -> "You got something!".
```

### Routes

Routes can be specified as records, or as function calls to `wtb_route:new/1-3`. Here is an example route which uses all of the available fields:

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
         prefix = [["this", "is"]]}
    ].
```

That single `#wtb_route{}` definition will result in the following webmachine routes getting created:

```
GET /this/is/my/route/#{var}
GET /this/is/my/route/#{var}/part/two/#{var2}
GET /this/is/my/alias/#{var}
GET /this/is/my/alias/#{var}/part/two/#{var2}
```

Here is the same route definition using `wtb_route:new`:

```
routes() ->
    SubRoute = route(myroute2, [["part", "two", var2]]),
    SubRoute1 = wtb_route:set_field(provides, [json], SubRoute),
    
    Route = wtb_route:new(
        myroute, 
        [["my", "route", var],["my", "alias", var]], 
        ['GET'])),
    Route1 = wtb_route:set_field(provides, [json], Route),
    Route2 = wtb_route:set_field(accepts, [any], Route1),
    Route3 = wtb_route:set_field(prefix, [["this", "is"]], Route2),
    [ Route3 ].
```

#### Route Fields

Name | Possible Values | Example
--- | --- | ---
**name** | `atom()` | `cluster`
**path** | `string()`, `atom()` | `[["path", "to", thing],["alias", "to", thing]]`
**methods** (optional) | `'GET'`, `'PUT'`, `'POST'`, `'DELETE'` | `['GET', 'PUT']`
**provides** (optional) | `json`, `binary`, `text`, `html`, `requested`, `any`, `none`, `string()` | `[json]`
**accepts** (optional) | `json`, `binary`, `text`, `html`, `requested`, `any`, `none`, `string()` | `["application/mycustomtype"]`
**routes** (optional) | `wtb_route()` | `[#wtb_route{name=simple}]` or `[wtb_route:new(simple)]`
**prefix** (optional) | `string()`, `atom()` | `[["prefix", "to", "route"],["alias_prefix", "to", "route"]]`
**resource** (optional, overrides) | `module()` | `webturbine_static_res`
**handlers** (optional) | `[{atom(), function()}]` | `[{myroute_get, fun(_,S)-> {"myroute_value",S} end}]`
**options** (optional) | `[{atom(), term()}]` | `[{mykey, <<"myvalue">>}]`

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

###### Available Callbacks Per Route: ######

Name | Description | Example
--- | --- | ---
**init** | Define the initial state of a request here. | `myroute_init() -> #state{}.`
**available** | Continues processing if `true`, renders `503` page if `false` | `myroute_available(_Req) -> true`
**exists** | Should return `true` or `false`, renders `404` if `false` | `myroute_exists(Req, State) -> Response=get_thing(wrq:path_info(thing, Req)), {true, State#state{response=Response}}.`
**get** | Should return the contents of an HTTP `GET` request | `myroute_get(Req, State=#state{response=Response}) -> {Response, State}.`
**put** | Should accept the body from an HTTP `PUT` request | `myroute_put(Req) -> put_thing(wrq:req_body(Req)), [{sucess, true}].`
**post_path** | If this function exists, then a `POST` is considered a create operation, and `post_path` should create the new resource name | `myroute_post_path() -> "node1".`
**post** | Should accept the body from an HTTP `POST` request | `myroute_put(Req) -> create_thing(wrq:req_body(Req)), true.`
**delete** | Should process an HTTP `DELETE` request | `myroute_delete(Req) -> delete_thing(wrq:path_info(thing, Req)), true.`
**last_modified** | Should return the last modified date of a resource for a route | `myroute_last_modified(Req) -> {{2021,1,1},{0,0,0}}.`
**etag** | Should return ETag header for a resource | `myroute_etag(Req, Response) -> {webmachine_util:quoted_string(hash_body(Response)), Response}`

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
    ClusterRoute = wtb_route:new(cluster, [["clusters", cluster]]),
    ClusterRoute1 = wtb_route:set_field(routes, [
        wtb_route:new(node, [["nodes", node]])
        ]),
    [ClusterRoute1].

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
