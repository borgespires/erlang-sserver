-module(acceptor).
-export([start_link/2]).

-export([accept/1]).

start_link(ListenSocket, I) ->
    Ref = list_to_atom("acceptor" ++ integer_to_list(I)),
    Pid = spawn_link(?MODULE, accept, [ListenSocket]),
    register(Ref, Pid),
    Pid.

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(req_handler, recv_and_respond, [Socket]);
        {error, closed} -> exit(closed);
        Error -> erlang:error(Error)
    end,
    accept(ListenSocket).