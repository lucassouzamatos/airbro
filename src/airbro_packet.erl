-module(airbro_packet).

-export([handle_binary/1]).

-include_lib("kernel/include/logger.hrl").

% Types of messages
-define(CONNECT, 16#04).
-define(CONNACK, 16#05).
-define(REGISTER, 16#0A).
-define(REGACK, 16#0B).
-define(DISCONNECT, 16#18).
-define(PUBLISH, 16#0C).
-define(PUBACK, 16#0D).

handle_binary(
    <<_Length:8, ?CONNECT:8, _Dup:1, _QoS:2, _Retain:1, _Will:1, _CleanSession:1, _TopicIdType:2,
        _ProtocolId:8, _Duration:16,
        _ClientId/binary>> =
        _Bin
) ->
    ?LOG_DEBUG("Packet::CONNECTED"),
    make_response_message(<<?CONNACK:8, 16#00:8>>);
% TODO: use TopicId when is sent by a GW
handle_binary(
    <<_Length:8, ?REGISTER:8, _TopicId:16, MsgId:16, TopicName/binary>> =
        _Bin
) ->
    ?LOG_DEBUG(string:join(["Packet", "TOPIC_REGISTER", TopicName], "::")),
    TopicId = airbro_gateway:register_topic(TopicName),
    make_response_message(<<?REGACK:8, TopicId:16, MsgId:16, 16#00:8>>);
% TODO: implement duration time to disconnect like in official documentation
handle_binary(<<_Length:8, ?DISCONNECT:8, _Duration/binary>> = _Bin) ->
    ?LOG_DEBUG("Packet::DISCONNECT"),
    make_response_message(<<?DISCONNECT:8>>);
handle_binary(
    <<_Length:8, ?PUBLISH:8, _Dup:1, QoS:2, _Retain:1, _Will:1, _CleanSession:1, _TopicIdType:2,
        TopicId:16, MsgId:16,
        Data/binary>> =
        _Bin
) ->
    ?LOG_DEBUG(string:join(["Packet", "PUBLISH", integer_to_list(TopicId), integer_to_list(MsgId), Data], "::")),
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
