-module(req_handler).
-export([recv_and_respond/1]).

-record(request, { method, path="", headers=[] }).

recv_and_respond(Socket) ->
    Request = recv_http(Socket),
    Response = create_response(Request),
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket).

recv_http(Socket) -> recv_http(Socket, #request{}).
recv_http(Socket, R=#request{}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, http_eoh} -> R;
        {ok, Data} ->
            case parse(Data) of
                {path, Method, Path} -> recv_http(Socket, R#request{method=Method, path=Path});
                {header, Header} -> recv_http(Socket, R#request{headers=[Header|R#request.headers]});
                _ -> recv_http(Socket, R)
            end;
        Error -> Error
    end.

parse({http_request, Method, {abs_path, Path}, _Version}) ->
    {path, Method, Path};
parse({http_header, _, Header, _, Val}) ->
    {header, {Header, Val}};
parse(_) -> ok.

create_response(R=#request{}) ->
    io:format("~s ~s~n", [R#request.method, R#request.path]),
    io:format("~s~n", [lists:flatten(io_lib:format("~p", [R#request.headers]))]),
    <<"HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!">>.