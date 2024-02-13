%%%-------------------------------------------------------------------
%% @doc airbro top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(airbro_sup).

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
            intensity => 0,
            period => 1
        },
    ChildSpecs =
        [
            #{
                id => airbro_gateway_sup,
                start => {airbro_gateway_sup, start_link, []},
                restart => permanent,
                shutdown => infinity,
                type => supervisor,
                modules => [airbro_gateway_sup]
            },
            #{
                id => airbro_broadcast_sup,
                start => {airbro_broadcast_sup, start_link, []},
                restart => permanent,
                shutdown => infinity,
                type => supervisor,
                modules => [airbro_broadcast_sup]
            }
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
