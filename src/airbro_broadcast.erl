-module(airbro_broadcast).

-behaviour(gen_server).

-export([init/1, start_link/0, handle_cast/2, handle_info/2, handle_call/3]).

-record(state, {sock :: any()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, Sock} = gen_udp:open(9800, [binary, {broadcast, true}]),
    {ok, #state{sock = Sock}}.

handle_call({start, Port}, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, _Bin} = Msg, State) ->
    io:format("server received:~p~n", [Msg]),
    gen_udp:send(Socket, Host, Port, term_to_binary(received)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
