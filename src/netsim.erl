-module(netsim).
-include("include/log_utils.hrl").
-include("include/netsim.hrl").

-behavior(application).

-export([start_app/0, start_app/1]).
-export([start/2, stop/1]).

start_app() ->
    start_app([
        filename:join(["priv", "nodelist.txt"]),
        filename:join(["priv", "channels.txt"]),
        filename:join(["priv", "simulation.txt"]),
        filename:join(["priv", "settings.txt"]),
        filename:join(["res", "ticks.txt"]),
        filename:join(["res", "total_traffic.txt"]),
        filename:join(["res", "traffic.txt"]),
        filename:join(["res", "traffic_histogram.txt"])
    ]).

start_app(Args=[NodeListFile, ChannelsFile, SimulationFile, SettingsFile,
        TicksFile, TotalTrafficFile, TrafficFile, TrafficHistogramFile]) ->

    ok = application:start(lager),
    % SimulationFile :: {Time, Queueid, Resource}
    {ok, SimulationList} = file:consult(SimulationFile),
    lists:foreach(
        fun (N) ->
                Simulations = lists:sublist(SimulationList, N),

                ok = application:start(netsim),
                % Test data:
                lager:info("Files: ~p", [Args]),
                netsim_stats:define_event(#stat{action=change, resource={kedainiai, 1}}),
                netsim_bootstrap:init(NodeListFile, ChannelsFile,
                    {Simulations, N}, SettingsFile, TicksFile,
                    TotalTrafficFile, TrafficFile, TrafficHistogramFile),

                ok = application:stop(netsim),
                lager:info("Running apps: ~p", [application:which_applications()]),
                timer:sleep(300)
        end,
        lists:seq(1, length(SimulationList))
    ).

start(_, _) ->
    {ok, _Pid} = netsim_sup:start_link().

stop(_State) ->
    ok.
