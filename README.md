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

### Routes

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

* init
* available
* get
* put
* post_path
* post
* delete
* last_modified
