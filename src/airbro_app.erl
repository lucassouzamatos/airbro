%%%-------------------------------------------------------------------
%% @doc airbro public API
%% @end
%%%-------------------------------------------------------------------

-module(airbro_app).

-behaviour(application).

-export([start/2, stop/1, subscribe/2]).
-export([add_topic_id/1, broadcast/0]).

-define(SERVER, ?MODULE).

start(_StartType, _StartArgs) ->
    airbro_sup:start_link().

stop(_State) ->
    ok.

subscribe(ClientId, TopicId) ->
    gen_server:call(airbro_gateway, {subscribe, {ClientId, TopicId}}).

add_topic_id(TopicId) ->
    gen_server:call(airbro_gateway, {add_topic_id, TopicId}).

broadcast() ->
    airbro_broadcast:start(9800).
