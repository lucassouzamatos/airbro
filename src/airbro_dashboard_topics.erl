-module(airbro_dashboard_topics).

-export([init/2]).
-export([content_types_provided/2]).
-export([run/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[{<<"text/html">>, run}, {<<"application/json">>, run}, {<<"text/plain">>, run}],
     Req,
     State}.

run(Req, State) ->
    Topics =   gen_server:call(airbro_gateway, {get_topics}),
    erlang:display(Topics),
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.
