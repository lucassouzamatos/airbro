-module(airbro_gateway).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/0]).
-export([register_topic/1, get_topic_id/1, subscribe/2, get_subscribed_clients/1]).

-define(SERVER, ?MODULE).
-define(TOPICS_TABLE, airbro_gateway_topics).
-define(TOPICS_CLIENTS_TABLE, airbro_gateway_topics_clients).

% public api
register_topic(Name) ->
    gen_server:call(?MODULE, {register_topic, Name}).

subscribe(Socket, TopicName) ->
    gen_server:call(?MODULE, {subscribe, {Socket, TopicName}}).

get_subscribed_clients(TopicId) ->
    gen_server:call(?MODULE, {get_subscribed_socket_clients, {TopicId}}).

get_topic_id(Name) ->
    gen_server:call(?MODULE, {get_topic_id, Name}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    log(gateway_started),
    create_tables(),
    {ok, []}.

handle_call({register_topic, Name}, _From, State) ->
    TopicId = insert_topic(Name),
    {reply, TopicId, State};
handle_call({subscribe, {Socket, TopicName}}, _From, State) ->
    TopicId = insert_topic(TopicName),
    subscribe_client(Socket, TopicId),
    {reply, TopicId, State};
handle_call({get_subscribed_socket_clients, {TopicId}}, _From, State) ->
    Subs = get_subscribed_socket_clients(TopicId),
    {reply, Subs, State};
handle_call({get_topics}, _From, State) ->
    Reply = ets:tab2list(?TOPICS_TABLE),
    {reply, Reply, State};
handle_call({get_topic_id, TopicName}, _From, State) ->
    Reply = ets:lookup_element(?TOPICS_TABLE, TopicName, 1),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% internal functions

%% table functions

lookup_topic(Name) ->
    case ets:lookup(?TOPICS_TABLE, Name) of
        [] ->
            {not_found};
        [Item] ->
            {found, Item}
    end.

create_tables() ->
    ets:new(?TOPICS_TABLE, [named_table, set, public]),
    ets:new(?TOPICS_CLIENTS_TABLE, [named_table, set, public]).

insert_topic(TopicName) ->
    case lookup_topic(TopicName) of
        {not_found} ->
            {_, [SeedValue | _]} = rand:seed(exsplus),
            TopicId = SeedValue rem 10000,
            true = ets:insert(?TOPICS_TABLE, {TopicName, TopicId}),
            TopicId;
        {found, {_Name, TopicId}} ->
            TopicId
    end.

subscribe_client(ClientSocket, TopicId) ->
    true = ets:insert(?TOPICS_CLIENTS_TABLE, {TopicId, ClientSocket}).

get_subscribed_socket_clients(TopicId) ->
    case ets:lookup(?TOPICS_CLIENTS_TABLE, TopicId) of
        [] ->
            not_found;
        Items ->
            [Item || {_, Item} <- Items]
    end.

%% logger functions

log(gateway_started) ->
    io:format("Airbro Gateway Server was started~n").

log(topic_added, [TopicId]) ->
    io:format("Topic ~s was added~n", [TopicId]);
log(topic_not_found, [TopicId]) ->
    io:format("Topic ~s was not found, you should created it before.~n", [TopicId]);
log(client_subscribed, [ClientId]) ->
    io:format("The client ~s was subscribed~n", [ClientId]).
