-module(netsim).
-include("include/log_utils.hrl").

-behavior(application).

-export([start_app/0, start_app/1]).
-export([start/2, stop/1]).

start_app() ->
    start_app([
        filename:join(["priv", "nodelist.txt"]),
        filename:join(["priv", "channels.txt"]),
        filename:join(["priv", "simulation.txt"]),
        filename:join(["priv", "max_latency.txt"])
    ]).

start_app(Args=[NodeList, Channels, Simulation, MaxLatency]) ->
    ok = application:start(lager),
    ok = application:start(netsim),

    % Test data:
    lager:info("Files: ~p", [Args]),
    netsim_bootstrap:init(NodeList, Channels, Simulation, MaxLatency).

start(_, _) ->
    {ok, _Pid} = netsim_sup:start_link().

stop(_State) ->
    ok.
