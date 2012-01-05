-module(netsim).

-behavior(application).

-export([start_app/0]).
-export([start/2, stop/1]).

start_app() ->
    application:start(netsim),

    netsim_bootstrap:init().

start(_, _) ->
    netsim_sup:start_link().

stop(_State) ->
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integration_test_() ->
    {foreach,
        fun start_app/0,
        fun cleanup/1,
        [
            {"Yadda test", fun yadda/0}
        ]
    }.

yadda() ->
    timer:sleep(100),
    ok.

cleanup(_) ->
    stop_app().

-endif.
