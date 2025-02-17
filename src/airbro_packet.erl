-module(airbro_packet).

-export([handle_binary/2]).

-include_lib("kernel/include/logger.hrl").

-define(CONNECT, 16#04).
-define(CONNACK, 16#05).
-define(REGISTER, 16#0A).
-define(REGACK, 16#0B).
-define(DISCONNECT, 16#18).
-define(PUBLISH, 16#0C).
-define(PUBACK, 16#0D).
-define(SUBSCRIBE, 16#12).
-define(SUBACK, 16#13).
-define(PINGREQ, 16#16).
-define(PINGRESP, 16#17).

handle_binary(<<_Length:8, PacketType:8, Rest/binary>> = Packet, Socket) ->
  case PacketType of
    ?CONNECT ->
      handle_connect(Rest);
    ?REGISTER ->
      handle_register(Rest);
    ?DISCONNECT ->
      handle_disconnect(Rest);
    ?PUBLISH ->
      handle_publish(Rest);
    ?SUBSCRIBE ->
      handle_subscribe(Rest, Socket);
    ?PINGREQ ->
      handle_pingreq(Rest);
    _ ->
      not_implemented
  end.

handle_connect(<<_Dup:1,
                 _QoS:2,
                 _Retain:1,
                 _Will:1,
                 _CleanSession:1,
                 _TopicIdType:2,
                 _ProtocolId:8,
                 _Duration:16,
                 _ClientId/binary>>) ->
  ?LOG_DEBUG("Packet::CONNECTED"),
  make_response_message(<<?CONNACK:8, 16#00:8>>).

handle_register(<<_TopicId:16, MsgId:16, TopicName/binary>>) ->
  ?LOG_DEBUG(string:join(["Packet", "TOPIC_REGISTER", TopicName], "::")),
  TopicId = airbro_gateway:register_topic(TopicName),
  make_response_message(<<?REGACK:8, TopicId:16, MsgId:16, 16#00:8>>).

handle_disconnect(_Duration) ->
  ?LOG_DEBUG("Packet::DISCONNECT"),
  make_response_message(<<?DISCONNECT:8>>).

handle_pingreq(<<ClientId/binary>>) ->
  ?LOG_DEBUG(string:join(["Packet", "PINGREQ", ClientId], "::")),
  make_response_message(<<?PINGRESP:8>>).

    %% TODO: implement duration time to disconnect like in official documentation

handle_subscribe(<<Dup:1,
                   QoS:2,
                   Retain:1,
                   Will:1,
                   CleanSession:1,
                   TopicIdType:2,
                   MsgId:16,
                   TopicName/binary>>,
                 Socket) ->
  ?LOG_DEBUG(string:join(["Packet",
                          "SUBSCRIBE",
                          integer_to_list(TopicIdType),
                          integer_to_list(MsgId),
                          TopicName],
                         "::")),

  TopicId = airbro_gateway:subscribe(Socket, TopicName),
  make_response_message(<<?SUBACK:8,
                          Dup:1,
                          QoS:2,
                          Retain:1,
                          Will:1,
                          CleanSession:1,
                          TopicIdType:2,
                          TopicId:16,
                          MsgId:16,
                          16#00:8>>).

handle_publish(<<_Dup:1,
                 QoS:2,
                 _Retain:1,
                 _Will:1,
                 _CleanSession:1,
                 _TopicIdType:2,
                 TopicId:16,
                 MsgId:16,
                 Data/binary>>) ->
  ?LOG_DEBUG(string:join(["Packet",
                          "PUBLISH",
                          integer_to_list(TopicId),
                          integer_to_list(MsgId),
                          Data],
                         "::")),
  Subscribers = airbro_gateway:get_subscribed_clients(TopicId),
  [#{socket := Socket,
     host := Host,
     port := Port}] =
    Subscribers,
  gen_udp:send(Socket,
               Host,
               Port,
               make_response_message(<<?PUBLISH:8,
                                       _Dup:1,
                                       QoS:2,
                                       _Retain:1,
                                       _Will:1,
                                       _CleanSession:1,
                                       _TopicIdType:2,
                                       TopicId:16,
                                       MsgId:16,
                                       Data/binary>>)),
  case QoS of
    0 ->
      skip;
    _ ->
      make_response_message(<<?PUBACK:8, TopicId:16, MsgId:16, 16#00:8>>)
  end.

make_response_message(Msg) ->
  MsgLength = byte_size(Msg),
  Len = MsgLength + byte_size(<<MsgLength>>),
  <<Len, Msg/binary>>.
