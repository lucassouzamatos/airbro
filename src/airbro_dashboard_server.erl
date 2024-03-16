-module(airbro_dashboard_server).

-export([setup/0]).

-record(route, {path :: string(), handler :: atom()}).

routes() ->
    [
        #route{path = "/topics", handler = airbro_dashboard_topics}
    ].

setup() ->
    airbro_server:setup(routes()),
    {ok, []}.
