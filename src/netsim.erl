-module(netsim).

-behavior(application).

-export([start_app/0]).
-export([start/2, stop/1]).

start_app() ->
    application:start(netsim).

start(_, _) ->
    netsim_sup:start_link().

stop(_State) ->
    ok.
