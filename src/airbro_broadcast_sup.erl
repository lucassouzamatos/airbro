%%%-------------------------------------------------------------------
%% @doc airbro broadcast supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(airbro_broadcast_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{
            strategy => one_for_all,
            intensity => 1,
            period => 1
        },
    ChildSpecs =
        [
            #{
                id => airbro_broadcast,
                restart => transient,
                start => {airbro_broadcast, start_link, []}
            }
        ],
    {ok, {SupFlags, ChildSpecs}}.
