-module(sserver).
-export([start_link/1]).
-export([accept/1, recv_loop/1]).

start_link(Port) ->
    case catch init(Port) of
        {ok, State} ->
            io:format("sserver ready on port ~b~n", [Port]),
            loop(State);
        Else ->
            Error = {bad_return_value, Else},
            exit(Error)
    end.

init(Port) ->
    Opts = [
        {active, false},
        binary,
        {backlog, 256},
        {packet, http_bin},
        {raw,6,9,<<1:32/native>>},
        {reuseaddr, true}
    ],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    Acceptors = spawn_acceptors(ListenSocket, 1),
    {ok, {ListenSocket, Acceptors}}.

spawn_acceptors(ListenSocket, N) ->
    [ { Id, spawn_acceptor(ListenSocket, Id) } || Id <- lists:seq(1, N) ].

spawn_acceptor(ListenSocket, I) ->
    Ref = list_to_atom("acceptor" ++ integer_to_list(I)),
    Pid = spawn_link(?MODULE, accept, [ListenSocket]),
    register(Ref, Pid),
    Pid.

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} -> 
            io:format("received new request~n"),
            spawn(?MODULE, recv_loop, [Socket]);
        {error, closed} -> exit(closed);
        Error -> erlang:error(Error)
    end,
    accept(ListenSocket).

recv_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, http_eoh} ->
            Response = <<"HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!">>,
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket),
            ok;
        {ok, _Data} -> recv_loop(Socket);
        Error -> Error
    end.

% trap acceptors exit messages and respawn
loop(State = {ListenSocket, _Acceptors}) ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', _, shutdown} -> exit(shutdown);
        {'EXIT', CrashedPid, _Reason} ->
            NewAcceptors = one_on_one(State, CrashedPid),
            loop({ListenSocket, NewAcceptors});
        _ -> loop(State)
    end.

one_on_one({ListenSocket, Acceptors}, Pid) ->
    lists:map(
        fun(Acceptor) -> respawn_if_crashed(ListenSocket, Pid, Acceptor) end, 
        Acceptors
    ).

respawn_if_crashed(ListenSocket, Pid, {I, Pid}) -> { I, spawn_acceptor(ListenSocket, I) };
respawn_if_crashed(_, _, Acceptor) -> Acceptor.