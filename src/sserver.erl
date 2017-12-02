-module(sserver).
-export([start_link/1]).
-export([accept/1, recv_and_respond/1]).

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
        %% If the value is true, everything received is sent as messages to the receiving process.
        %% If the value is false (passive mode), the process must explicitly receive incoming data by calling gen_tcp:recv/2,3,
        {active, false}, % blocks on `:gen_tcp.recv/2` until data is available
        binary, % receives data as binaries (instead of lists)
        {backlog, 256}, %  maximum pending connections on the queue
        {packet, http_bin}, % how to deal with the package (header size, how to parse)
        % {raw,6,9,<<1:32/native>>}, % pass some option flags directly to the kernel (?what can we do with this)
        {reuseaddr, true} % allows to reuse the address if the listener crashes
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
            spawn(?MODULE, recv_and_respond, [Socket]);
        {error, closed} -> exit(closed);
        Error -> erlang:error(Error)
    end,
    accept(ListenSocket).

recv_and_respond(Socket) ->
    Request = sshelper:recv_http(Socket),
    Response = sshelper:create_response(Request),
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket).

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