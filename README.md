# Webturbine

An Erlang web resource library for faster and less verbose API development.

## Rebar Configuration

In your rebar.config, ensure the following is there:

```
{deps, [
    {webturbine, {git, "https://github.com/drewkerrigan/webturbine.git", {branch, "master"}}}
]}.
```

# Cowboy Integration #

Webturbine relies on [Cowboy](https://github.com/ninenines/cowboy). Here's an example resource dispatch:

```
%% Start HTTP listeners
DispatchList = webturbine:dispatch([myresource]),
Dispatch = cowboy_router:compile([{'_', DispatchList}]),
{ok, _} = cowboy:start_clear(http, 10, [{port, 8080}], #{env => #{dispatch => Dispatch}}).
```

## Resource Definition

### Behaviour

Your resource module should implement the `webturbine_resource` behaviour like so:

```
-module(myresource).
-behaviour(webturbine_resource). %% Optional
-export([
  something_get/0,
  routes/0
  ]).

%% Behaviour Callbacks:
routes() -> [wtb_route:new(something)].

%% Route Callbacks:
something_get() -> "You got something!".
```

### Routes

Routes can be specified as records, or as function calls to `wtb_route:new/1-3`. Here is an example route which uses all of the available fields:

```
-include_lib("webturbine/include/webturbine.hrl").

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
                 name = mysubroute,
                 path = [["part", "two", var2]],
                 provides = [json]
         ],
         prefix = [["this", "is"]]},
         handlers = [{myroute_get, fun(_,S)-> {"myroute GET response!",S} end}],
         resource = myroute_module,
         state = {my, initial, <<"state">>},
         handler_type = rest
     }
    ].
```

That `#wtb_route{}` definition will result in the following webmachine routes getting created:

```
GET /this/is/my/route/#{var}
GET /this/is/my/route/#{var}/part/two/#{var2}
GET /this/is/my/alias/#{var}
GET /this/is/my/alias/#{var}/part/two/#{var2}
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
**state** (optional) | `term()` | `[{mykey, <<"myvalue">>}]`
**handler_type** (optional) | `rest | websocket` | `rest`

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
**is_conflict** | Should return `true` or `false`, renders `409` if `true` | `myroute_is_conflict(Req, State) -> myroute_exists(Req, State).`
**previously_existed** | Should return `true` or `false` | `myroute_peviously_existed(Req, State) -> myroute_exists(Req, State).`
**malformed_request** | Should return `true` or `false` | `myroute_malformed_request(Req, State) -> {valid_req(Req), State}.`
**get** | Should return the contents of an HTTP `GET` request | `myroute_get(Req, State=#state{response=Response}) -> {Response, State}.`
**put** | Should accept the body from an HTTP `PUT` request | `myroute_put(Req) -> put_thing(wrq:req_body(Req)), [{sucess, true}].`
**patch** | Should accept the body from an HTTP `PATCH` request | `myroute_patch(Req) -> patch_thing(wrq:req_body(Req)), [{sucess, true}].`
**post_path** | If this function exists, then a `POST` is considered a create operation, and `post_path` should create the new resource name | `myroute_post_path() -> "node1".`
**post** | Should accept the body from an HTTP `POST` request | `myroute_put(Req) -> create_thing(wrq:req_body(Req)), true.`
**delete** | Should process an HTTP `DELETE` request | `myroute_delete(Req) -> delete_thing(wrq:path_info(thing, Req)), true.`
**last_modified** | Should return the last modified date of a resource for a route | `myroute_last_modified(Req) -> {{2021,1,1},{0,0,0}}.`
**etag** | Should return ETag header for a resource | `myroute_etag(Req, State) -> {generate_etag(State)), State}`
**handle** | (Websocket routes only, use 'handler_type=websocket') | `mysocket_handle({text, Msg}) -> {text, << "Great, how about you! ", Msg/binary >>}.`
**info** | (Websocket routes only, use 'handler_type=websocket') | `mysocket_info({timeout, _Ref, Msg}) -> {text, Msg}`

### Example Rest Resource

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


### Example Websocket Resource

`websocket_res.erl`:

```
-module(websocket_res).
-behaviour(webturbine_resource).
-export([mysocket_handle/1,
         mysocket_info/1]).

routes() -> 
    [
      #wtb_route{name = mysocket,
                path = [["mysocket"]],
                handler_type = websocket}
    ].

mysocket_init() ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	ok.

mysocket_handle({text, Msg}) ->
	{text, <<"You sent me: ", Msg/binary >>}.

mysocket_info({timeout, _Ref, Msg}) ->
	erlang:start_timer(1000, self(), <<"Hello?">>),
    {text, Msg}.
```
