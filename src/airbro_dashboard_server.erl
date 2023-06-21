-module(airbro_dashboard_server).

-export([setup/0]).

start_dependencies() ->
    application:start(ranch),
    application:start(cowboy).

setup() ->
    start_dependencies(),

    Routes = [{'_', [{"/topics", airbro_dashboard_topics, []}]}],
    Dispatch = cowboy_router:compile(Routes),
    Env = #{env => #{dispatch => Dispatch}},
    Opts = [{ip, {0, 0, 0, 0}}, {port, 2938}],

    cowboy:start_clear(airbro_dashboard_rest_api, Opts, Env),

    {ok, []}.
