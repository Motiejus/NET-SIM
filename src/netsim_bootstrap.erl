%% @doc Starts simulation.
-module(netsim_bootstrap).

-include("include/netsim.hrl").

-export([init/8]).

%% @doc Reads data from files and creates new nodes setup.
-spec init(list(), list(), list(), list(), list(), list(), list(), list()) ->
    no_return().
init(NodesFiles, LinksFile, SimulationFile, SettingsFile, TicksFile,
        TotalTrafficFile, TrafficFile, TrafficHistogramFile) ->
    % NodeList :: {NodeId, Price}
    {ok, NodesList} = file:consult(NodesFiles),
    % LinksList :: [{From, To, Latency, Bandwith}]
    {ok, LinksList} = file:consult(LinksFile),
    % SimulationFile :: {Time, Queueid, Resource}
    {ok, SimulationList} = file:consult(SimulationFile),
    % LatencyFile :: MaxLatency
    {ok, Settings} = file:consult(SettingsFile),

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

    {NodeId, _} = proplists:get_value(monitor_resource, Settings),

    % Join stats group:
    pg2:join(?NETSIM_PUBSUB, self()),

    lists:foreach(
        fun (N) ->
            Simulation = lists:nth(N, SimulationList),
            lager:info("Starting ~p-nth simulation (~p)", [N, Simulation]),

            Res = Simulation#event.resource,
            SimulationAction =
                case Simulation#event.action of
                    add -> change;
                    del -> del
                end,

            %netsim_stats:define_event(#stat{action=change, resource=Res, nodeid=NodeId}),
            netsim_stats:define_event(#stat{action=SimulationAction, resource=Res}),

            Postfix = "_" ++ hd(io_lib:format("~p", [N])) ++ ".txt",

            netsim_clock_serv:start(Simulation), % Starts ticking

            % Wait for 'finished' from stats:
            receive
                finished ->
                    lager:info("Finished stats."),
                    Log = netsim_stats:log(),

                    % Write ticks log:
                    {ok, Dev0} = file:open([TicksFile, Postfix], [write]),
                    TotalNodes = length(Log#log.traffic),
                    ok = io:fwrite(Dev0, "#tick count~n", []),
                    lists:foldr(
                        fun ({Tick, Count}, Acc) ->
                                NewVal = (Count + Acc) / TotalNodes * 100,
                                ok = io:fwrite(Dev0, "~p ~.2f~n", [Tick, NewVal]),
                                Count + Acc
                        end,
                        0,
                        Log#log.ticks
                    ),
                    ok = file:close(Dev0),

                    % Write total traffic log:
                    {ok, Dev1} = file:open([TotalTrafficFile, Postfix], [write]),
                    ok = io:fwrite(Dev1, "#tick tx rx~n", []),
                    lists:foreach(
                        fun ({Tick, {TX, RX}}) ->
                            ok = io:fwrite(Dev1, "~p ~p ~p~n", [Tick, TX, RX])
                        end,
                        Log#log.total_traffic
                    ),
                    ok = file:close(Dev1),

                    % Write traffic log:
                    {ok, Dev2} = file:open([TrafficFile, Postfix], [write]),
                    ok = io:fwrite(Dev2, "#node_id tx rx~n", []),
                    MaxRxTx = lists:foldl(
                        fun ({Node, {TX, RX}}, MaxSoFar) ->
                            io:fwrite(Dev2, "~p ~p ~p~n", [Node, TX, RX]),
                            max(MaxSoFar, RX+TX)
                        end,
                        0,
                        Log#log.traffic
                    ),
                    ok = file:close(Dev2),

                    Intervals = 10,
                    % Split data to 10 intervals
                    Step = MaxRxTx div Intervals,
                    Histogram = lists:sort(lists:foldl(
                            fun ({_Node, {TX, RX}}, Acc) ->
                                    Interval = trunc(Intervals*(TX + RX)/MaxRxTx) * Step,
                                    Num = proplists:get_value(Interval, Acc, 0),
                                    set_value(Interval, Num+1, Acc)
                            end,
                            [],
                            Log#log.traffic
                        )
                    ),

                    % Write traffic histogram:
                    {ok, Dev3} = file:open([TrafficHistogramFile, Postfix], [write]),
                    [ok = io:fwrite(Dev3, "~p ~p~n", [T,N1]) || {T,N1} <- Histogram],
                    ok = file:close(Dev3),

                    lager:info("EOF")
            end
        end,
        lists:seq(1, length(SimulationList))
    ).

set_value(K, V, Proplist) ->
    [{K, V}|proplists:delete(K, Proplist)].
