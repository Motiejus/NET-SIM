%% @doc

-module(netsim_bootstrap).

-export([init/0, init/3]).

%% @doc Runs init with args.`
init() ->
    init("./priv/nodelist.txt",
        "./priv/channels.txt",
        "./priv/simulation.txt").

%% @doc Reads data from files and creates new nodes setup.
%% @todo Rename Channel to Link.
-spec init(list(), list(), list()) -> no_return().
init(NodesFiles, ChannelsFile, SimulationFile) ->
    % NodeList :: {NodeId, Price}
    {ok, NodesList} = file:consult(NodesFiles),
    % ChannelsList :: [{From, To, Latency, Bandwith}]
    {ok, ChannelsList} = file:consult(ChannelsFile),
    % SimulationFile :: {Time, Queueid, Resource}
    {ok, SimulationFile} = file:consult(SimulationFile),

    % Send SimulationFile to clock_serv
    netsim_clock_serv:send_data_file(SimulationFile),

    % Start nodes without channels:
    [netsim_sup:add_node(Id, Price) || {Id, Price} <- NodesList],

    % Init channels:
    lists:foreach(
        fun ({NodeId0, NodeId1, _, _}=Link) ->
            ok = netsim_serv:add_link(NodeId0, Link),
            ok = netsim_serv:add_link(NodeId1, Link)
        end,
        ChannelsList
    ).
