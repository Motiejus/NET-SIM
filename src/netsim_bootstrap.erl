%% @doc
-include("include/log_utils.hrl").

-module(netsim_bootstrap).

-export([init/0, init/4]).

%% @doc Runs init with args.`
init() ->
    init(
        filename:join([code:priv_dir(netsim), "nodelist.txt"]),
        filename:join([code:priv_dir(netsim), "channels.txt"]),
        filename:join([code:priv_dir(netsim), "simulation.txt"]),
        filename:join([code:priv_dir(netsim), "max_latency.txt"])
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
        fun ({NodeId0, NodeId1, _, _}=Link) ->
            ok = netsim_serv:add_link(NodeId0, Link),
            ok = netsim_serv:add_link(NodeId1, Link)
        end,
        ChannelsList
    ),

    netsim_clock_serv:start(). % Starts ticking
