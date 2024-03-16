-module(airbro_server).
-export([setup/1]).
-record(route, {path :: string(), handler :: atom()}).

start_dependencies() ->
    application:start(ranch),
    application:start(cowboy).

build_routes(Routes) ->
    Routes_1 = [{R#route.path, R#route.handler, []} || R <- Routes],
    [{'_', Routes_1}].

setup(Routes) ->
    start_dependencies(),

    Routes_1 = build_routes(Routes),
    Dispatch = cowboy_router:compile(Routes_1),
    Env = #{env => #{dispatch => Dispatch}},
    Opts = [{ip, {0, 0, 0, 0}}, {port, 2939}],

    cowboy:start_clear(airbro_dashboard_rest_api, Opts, Env),

    {ok, []}.
