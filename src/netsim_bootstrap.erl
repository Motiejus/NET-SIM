%% @doc

-module(netsim_bootstrap).

-export([init/2]).

%% @doc Reads data from files and creates new nodes setup.
-spec init(list(), list()) -> {}.
init(NodesFiles, ChannelsFile) ->
    {ok, NodesList} = file:consult(NodesFiles),
    {ok, ChannelsList} = file:consult(ChannelsFile),

    % Start nodes without channels:
    [netsim_sup:add_node(Id, Cost) || {Id, Cost} <- NodesList],

    % Init channels:
    % @todo
    ok.
