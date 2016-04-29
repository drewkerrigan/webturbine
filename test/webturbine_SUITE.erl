-module(webturbine_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(EPHEMERAL_PORT, 0).

-record(integration_state, {
          webmachine_sup,
          mochi_serv,
          port,
          base_url = "http://localhost"
         }).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    ibrowse:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [route_test].

route_test() -> 
    [].

route_test(_Config) -> 
    DL = webturbine:dispatch([webturbine_test_resource]),
    Context = start_server("127.0.0.1", DL),

    {ok, "200", _, "something"} = 
        ibrowse:send_req(url(Context, "base/echo/something"), [], get, [], []),
    {ok, "404", _, _} = 
        ibrowse:send_req(url(Context, "clusters/nothere"), [], get, [], []),
    {ok, "200", _, "here"} = 
        ibrowse:send_req(url(Context, "clusters/here"), [], get, [], []),
    {ok, "404", _, _} = 
        ibrowse:send_req(url(Context, "clusters/nothere/nodes/any"), [], get, [], []),
    {ok, "200", _, "Cluster: here, Node: mynode"} = 
        ibrowse:send_req(url(Context, "clusters/here/nodes/mynode"), [], get, [], []),
    {ok, "200", _, "{\"its\":\"json\"}"} = 
        ibrowse:send_req(url(Context, "short"), [], get, [], []),

    stop_server(Context),
    ok.
    

%%====================================================================
%% Internal functions
%%====================================================================

start_server(IP, DispatchList) ->
    cleanup_previous_runs(),
    error_logger:tty(false),
    application:start(inets),
    {ok, WebmachineSup} = webmachine_sup:start_link(),
    WebConfig = [{ip, IP},
                 {port, ?EPHEMERAL_PORT},
                 {nodelay, true},
                 {dispatch, DispatchList}],
    {ok, MochiServ} = webmachine_mochiweb:start(WebConfig),
    link(MochiServ),
    Port = mochiweb_socket_server:get(MochiServ, port),
    #integration_state{webmachine_sup=WebmachineSup,
                       mochi_serv=MochiServ,
                       port=Port}.

stop_server(Context) ->
    stop_supervisor(Context#integration_state.webmachine_sup),
    MochiServ = Context#integration_state.mochi_serv,
    {registered_name, MochiName} = process_info(MochiServ, registered_name),
    webmachine_mochiweb:stop(MochiName),
    stop_supervisor(MochiServ),
    application:stop(inets).

%% Receives the integration_state record returned by start_webmachine, returns
%% the port to use to communicate with Webmachine over HTTP.
get_port(Context) ->
    Context#integration_state.port.

url(Context) ->
    Port = get_port(Context),
    Chars = io_lib:format("http://127.0.0.1:~b", [Port]),
    lists:flatten(Chars).

url(Context, Path) ->
    url(Context) ++ "/" ++ Path.

stop_supervisor(Sup) ->
    unlink(Sup),
    exit(Sup, kill),
    wait_for_pid(Sup).

wait_for_pid(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mref, process, _, _} ->
            ok
    after
        5000 ->
            {error, didnotexit, Pid, erlang:process_info(Pid)}
    end.

cleanup_previous_runs() ->
    RegNames = [webmachine_sup, webmachine_router, webmachine_logger,
                webmachine_log_event, webmachine_logger_watcher_sup],
    UndefinedsOrPids = [whereis(RegName) || RegName <- RegNames],
    [wait_for_pid(Pid) || Pid <- UndefinedsOrPids, Pid /= undefined].
