-module(airbro_broadcast).

-export([start/1]).

start(Port) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {broadcast, true}]),
    listen(Sock).

listen(Socket) ->
    receive
        {udp, Socket, Host, Port, _Bin} = Msg ->
            io:format("server received:~p~n", [Msg]),
            gen_udp:send(Socket, Host, Port, term_to_binary(any)),
            listen(Socket)
    end.
