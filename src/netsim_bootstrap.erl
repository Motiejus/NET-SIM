%% @doc Starts simulation.
-module(netsim_bootstrap).

-include("include/netsim.hrl").

-export([init/5]).

%% @doc Reads data from files and creates new nodes setup.
-spec init(list(), list(), list(), list(), list()) -> no_return().
init(NodesFiles, LinksFile, SimulationFile, SettingsFile, Output) ->
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
            {ok, Dev} = file:open(Output, [write]),

            Log = netsim_stats:log(),
            % * Log#log.events - [S1, S2], S1 - converge event, S2 - route update
            %   event;
            % * Log#log.traffic - [{NodeId, TX+RX}];
            % * Log#log.ticks - [{Tick, Count}], starts from send_event event.
            lager:info("Log: ~p", [Log])
    end,

    ok.
    %receive
    %    {done, TickLog} ->
    %        % TickLog :: [{latency(), integer()}]
    %        {ok, Dev} = file:open(Output, [write]),
    %        TotalNodes = length(NodesList),

    %        % @todo UGLY HACK. Make initial resource add/del to TickLog!
    %        [#event{time=Start}|_] = SimulationList,

    %        lager:info("Start: ~p", [Start]),
    %        ok = io:fwrite(Dev, "~p 0.00~n", [Start]),
    %        lists:foldr(
    %            fun ({Time, HowMany}, Acc) ->
    %                    NewVal = (HowMany + Acc) / TotalNodes * 100,
    %                    ok = io:fwrite(Dev, "~p ~.2f~n", [Time, NewVal]),
    %                    HowMany + Acc
    %            end,
    %            0,
    %            lists:sublist(TickLog, TotalNodes)
    %        ),
    %        ok = file:close(Dev),
    %        lager:info("Output written to ~p", [Output]),
    %        init:stop()
    %end.
