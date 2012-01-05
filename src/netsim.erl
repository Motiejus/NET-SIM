-module(netsim).
-include("include/log_utils.hrl").

-behavior(application).

-export([start_app/0, stop_app/0]).
-export([start/2, stop/1]).

start_app() ->
    application:start(netsim).

stop_app() ->
    application:stop(netsim).

start(_, _) ->
    {ok, Pid} = netsim_sup:start_link(),
    netsim_bootstrap:init(),
    {ok, Pid}.

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
