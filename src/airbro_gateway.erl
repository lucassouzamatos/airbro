-module(airbro_gateway).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/0]).

-define(SERVER, ?MODULE).
-define(TOPICS_TABLE, airbro_gateway_topics).
-define(TOPICS_CLIENTS_TABLE, airbro_gateway_topics_clients).

-record(topic_client, {client :: atom(), topic :: atom()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    log(gateway_started),
    create_tables(),
    {ok, []}.

handle_call({add_topic_id, TopicId}, _From, State) ->
    insert_topic_id(TopicId),
    {reply, log(topic_added, [TopicId]), State};
handle_call({subscribe, {ClientId, TopicId}}, _From, State) ->
    Reply =
        case lookup_topic(TopicId) of
            not_found ->
                log(topic_not_found, [TopicId]);
            _ ->
                subscribe_client(ClientId, TopicId),
                log(client_subscribed, [ClientId])
        end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% internal functions

%% table functions

lookup_topic(TopicId) ->
    case ets:lookup(?TOPICS_TABLE, TopicId) of
        [] ->
            not_found;
        [_] ->
            found
    end.

create_tables() ->
    ets:new(?TOPICS_TABLE, [named_table, set, public]),
    ets:new(?TOPICS_CLIENTS_TABLE, [named_table, set, public]).

insert_topic_id(TopicId) ->
    true = ets:insert(?TOPICS_TABLE, {TopicId}).

subscribe_client(ClientId, TopicId) ->
    true =
        ets:insert(?TOPICS_CLIENTS_TABLE, #topic_client{client = ClientId, topic = TopicId}).

%% logger functions

log(gateway_started) ->
    io:format("Airbro Gateway Server was started~n").

log(topic_added, [TopicId]) ->
    io:format("Topic ~s was added~n", [TopicId]);
log(topic_not_found, [TopicId]) ->
    io:format("Topic ~s was not found, you should created it before.~n", [TopicId]);
log(client_subscribed, [ClientId]) ->
    io:format("The client ~s was subscribed~n", [ClientId]).
