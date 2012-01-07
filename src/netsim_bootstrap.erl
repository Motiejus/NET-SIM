%% @doc Starts simulation.
-module(netsim_bootstrap).

-include("include/netsim.hrl").

-export([init/7]).

%% @doc Reads data from files and creates new nodes setup.
-spec init(list(), list(), list(), list(), list(), list(), list()) ->
    no_return().
init(NodesFiles, LinksFile, SimulationFile, SettingsFile, TicksFile,
        TotalTrafficFile, TrafficFile) ->
    % NodeList :: {NodeId, Price}
    {ok, NodesList} = file:consult(NodesFiles),
    % LinksList :: [{From, To, Latency, Bandwith}]
    {ok, LinksList} = file:consult(LinksFile),
    % SimulationFile :: {Time, Queueid, Resource}
    {ok, SimulationList} = file:consult(SimulationFile),
    % LatencyFile :: MaxLatency
    {ok, Settings} = file:consult(SettingsFile),

    % Send SimulationFile to clock_serv
    Rcpt = self(),
    netsim_clock_serv:initialize(
        {
            SimulationList,
            fun(Res) -> Rcpt ! {done, Res} end
        }
    ),

    % Start nodes without channels:
    MaxLatency = proplists:get_value(max_latency, Settings),
    [netsim_sup:add_node(Id, Price, MaxLatency) || {Id, Price} <- NodesList],

    % Init channels:
    lists:foreach(
        fun ({NodeId0, NodeId1, Latency1, Bandwidth1}) ->
            Link1 = {NodeId0, NodeId1,
                [{latency, Latency1}, {bandwidth, Bandwidth1}]},
            ok = netsim_serv:add_link(NodeId0, Link1),
            ok = netsim_serv:add_link(NodeId1, Link1)
        end,
        LinksList
    ),

    Res = {NodeId, _} = proplists:get_value(monitor_resource, Settings),
    netsim_stats:define_event(#stat{action=change, resource=Res, nodeid=NodeId}),

    % Join stats group:
    pg2:join(?NETSIM_PUBSUB, self()),

    netsim_clock_serv:start(), % Starts ticking

    % Wait for 'finished' from stats:
    receive
        finished ->
            lager:info("Finished stats."),
            Log = netsim_stats:log(),

            % Write ticks log:
            {ok, Dev0} = file:open(TicksFile, [write]),
            lists:foreach(
                fun ({Tick, Count}) ->
                    ok = io:fwrite(Dev0, "~p ~p~n", [Tick, Count])
                end,
                Log#log.ticks
            ),
            ok = file:close(Dev0),

            % Write total traffic log:
            {ok, Dev1} = file:open(TotalTrafficFile, [write]),
            lists:foreach(
                fun ({Tick, {TX, RX}}) ->
                    ok = io:fwrite(Dev1, "~p ~p ~p~n", [Tick, TX, RX])
                end,
                Log#log.total_traffic
            ),
            ok = file:close(Dev1),

            % Write traffic log:
            {ok, Dev2} = file:open(TrafficFile, [write]),
            lists:foreach(
                fun ({Node, {TX, RX}}) ->
                    ok = io:fwrite(Dev2, "~p ~p ~p~n", [Node, TX, RX])
                end,
                Log#log.traffic
            ),
            ok = file:close(Dev2),

            lager:info("EOF")
    end.
