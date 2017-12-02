-module(sshelper).
-export([recv_http/1, create_response/1]).

-record(request, { method, path="", headers=[] }).

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

parse({http_request, Method, Path, _Version}) ->
    {path, Method, from_ascii(Path)};
parse({http_header, _, Header, _, Val}) ->
    {header, {Header, Val}};
parse(_) -> ok.

from_ascii({abs_path, Bin}) -> io_lib:format("~s~n", [Bin]).

create_response(R=#request{}) ->
    io:fwrite(R#request.method),
    io:fwrite(R#request.path),
    lists:map(fun debug/1, R#request.headers),
    <<"HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!">>.


% debug
debug(Data) -> io:format("~w~n", [Data]).