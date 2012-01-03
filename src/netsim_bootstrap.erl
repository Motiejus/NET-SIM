%% @doc

-module(netsim_bootstrap).

-export([init/0, init/2]).

%% @doc Runs init with args.`
init() ->
    init("./priv/nodelist.txt", "./priv/channels.txt").

%% @doc Reads data from files and creates new nodes setup.
-spec init(list(), list()) -> {}.
init(NodesFiles, ChannelsFile) ->
    % NodeList :: [{NodeId, Cost}]
    {ok, NodesList} = file:consult(NodesFiles),
    % ChannelsList :: [{From, To, Latency, Bandwith}]
    {ok, ChannelsList} = file:consult(ChannelsFile),

    % Start nodes without channels:
    [netsim_sup:add_node(Id, Cost) || {Id, Cost} <- NodesList],

    % Init channels:
    lists:foreach(
        fun ({NodeId0, NodeId1, _, _}=Channel) ->
            ok = netsim_serv:add_channel(NodeId0, Channel),
            ok = netsim_serv:add_channel(NodeId1, Channel)
        end,
        ChannelsList
    ).
