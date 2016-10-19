-module(webturbine_ws_client).

-export([
         start_link/1,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link(From) ->
    io:format("Start Link ~p~n", [From]),
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("ws://localhost:10001/socket", ?MODULE, [From]).

init([From], _ConnState) ->
    io:format("Init ~p~n", [From]),
    ok = websocket_client:cast(self(), {text, <<"message 1">>}),
    {ok, {2, From}}.

websocket_handle({pong, _}, _ConnState, State={_, From}) ->
    io:format("Received pong~n", []),
    From ! done,
    {ok, State};
websocket_handle({text, Msg}, _ConnState, {3, From}) ->
    io:format("Received last msg ~p~n", [Msg]),
    From ! Msg,
    {close, <<>>, {10, From}};
websocket_handle({text, Msg}, _ConnState, {Num, From}) ->
    io:format("Received msg ~p~n", [Msg]),
    From ! Msg,
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(Num)),
    {reply, {text, <<"hello, this is message #", BinInt/binary >>}, {Num + 1, From}}.

websocket_info(start, _ConnState, State) ->
    io:format("Received erlang start ~n", []),
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih code ~p and payload ~p~n",
              [State, Code, Payload]),
    ok.
