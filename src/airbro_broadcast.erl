-module(airbro_broadcast).

-behaviour(gen_server).

-export([init/1, start_link/0, handle_cast/2, handle_info/2, handle_call/3]).

-record(state, {sock :: any()}).

-define(CONNECT, 16#04).
-define(CONNACK, 16#05).
-define(REGISTER, 16#0A).
-define(REGACK, 16#0B).
-define(DISCONNECT, 16#18).
-define(PUBLISH, 16#0C).
-define(PUBACK, 16#0D).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, Sock} = gen_udp:open(9801, [binary, {broadcast, true}]),
    {ok, #state{sock = Sock}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, Bin} = _Msg, State) ->
    Response = airbro_packet:handle_binary(Bin),
    case Response of
        not_implemented ->
            ok;
        skip ->
            ok;
        R ->
            gen_udp:send(Socket, Host, Port, R)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
