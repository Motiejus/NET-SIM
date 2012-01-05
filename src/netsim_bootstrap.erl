%% @doc
-include("include/log_utils.hrl").

-module(netsim_bootstrap).

-export([init/0, init/4]).

%% @doc Runs init with args.`
init() ->
    init(
        filename:join(["..", "priv", "nodelist.txt"]),
        filename:join(["..", "priv", "channels.txt"]),
        filename:join(["..", "priv", "simulation.txt"]),
        filename:join(["..", "priv", "max_latency.txt"])
    ).

%% @doc Reads data from files and creates new nodes setup.
%% @todo Rename Channel to Link.
-spec init(list(), list(), list(), list()) -> no_return().
init(NodesFiles, ChannelsFile, SimulationFile, LatencyFile) ->
    % NodeList :: {NodeId, Price}
    {ok, NodesList} = file:consult(NodesFiles),
    % ChannelsList :: [{From, To, Latency, Bandwith}]
    {ok, ChannelsList} = file:consult(ChannelsFile),
    % SimulationFile :: {Time, Queueid, Resource}
    {ok, SimulationList} = file:consult(SimulationFile),
    % LatencyFile :: MaxLatency
    {ok, [MaxLatency]} = file:consult(LatencyFile),

    % Send SimulationFile to clock_serv
    netsim_clock_serv:send_data_file(SimulationList),

    % Start nodes without channels:
    [netsim_sup:add_node(Id, Price, MaxLatency) || {Id, Price} <- NodesList],

    % Init channels:
    lists:foreach(
        fun ({NodeId0, NodeId1, Latency1, Bandwidth1}=Link) ->
            Link1 = {NodeId0, NodeId1,
                [{latency, Latency1}, {bandwidth, Bandwidth1}]},
            ok = netsim_serv:add_link(NodeId0, Link1),
            ok = netsim_serv:add_link(NodeId1, Link1)
        end,
        ChannelsList
    ),

    netsim_clock_serv:start(). % Starts ticking
