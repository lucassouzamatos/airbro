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
    {ok, Sock} = gen_udp:open(9800, [binary, {broadcast, true}]),
    {ok, #state{sock = Sock}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, Bin} = _Msg, State) ->
    Response = handle_binary(Bin),
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

%% internal functions
handle_binary(<<_Length:8,
                ?CONNECT:8,
                _Dup:1,
                _QoS:2,
                _Retain:1,
                _Will:1,
                _CleanSession:1,
                _TopicIdType:2,
                _ProtocolId:8,
                _Duration:16,
                _ClientId/binary>> =
                  _Bin) ->
    make_response_message(<<?CONNACK:8, 16#00:8>>);
% TODO: use TopicId when is sent by a GW
handle_binary(<<_Length:8, ?REGISTER:8, _TopicId:16, MsgId:16, _TopicName/binary>> =
                  _Bin) ->
    TopicIdResponse = 88,
    make_response_message(<<?REGACK:8, TopicIdResponse:16, MsgId:16, 16#00:8>>);
% TODO: implement duration time to disconnect like in official documentation
handle_binary(<<_Length:8, ?DISCONNECT:8, _Duration/binary>> = _Bin) ->
    make_response_message(<<?DISCONNECT:8>>);
handle_binary(<<_Length:8,
                ?PUBLISH:8,
                _Dup:1,
                QoS:2,
                _Retain:1,
                _Will:1,
                _CleanSession:1,
                _TopicIdType:2,
                TopicId:16,
                MsgId:16,
                Data/binary>> =
                  _Bin) ->
    case QoS of
        0 ->
            skip;
        _ ->
            make_response_message(<<?PUBACK:8, TopicId:16, MsgId:16, 16#00:8>>)
    end;
handle_binary(_) ->
    not_implemented.

make_response_message(Msg) ->
    MsgLength = byte_size(Msg),
    Len = MsgLength + byte_size(<<MsgLength>>),
    <<Len, Msg/binary>>.
