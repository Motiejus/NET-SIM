%% @doc Starts simulation.

-include("include/log_utils.hrl").

-module(netsim_bootstrap).

-export([init/4]).

%% @doc Reads data from files and creates new nodes setup.
-spec init(list(), list(), list(), list()) -> no_return().
init(NodesFiles, LinksFile, SimulationFile, LatencyFile) ->
    % NodeList :: {NodeId, Price}
    {ok, NodesList} = file:consult(NodesFiles),
    % LinksList :: [{From, To, Latency, Bandwith}]
    {ok, LinksList} = file:consult(LinksFile),
    % SimulationFile :: {Time, Queueid, Resource}
    {ok, SimulationList} = file:consult(SimulationFile),
    % LatencyFile :: MaxLatency
    {ok, [MaxLatency]} = file:consult(LatencyFile),

    % Send SimulationFile to clock_serv
    Rcpt = self(),
    netsim_clock_serv:initialize(
        {
            SimulationList,
            fun(_) -> lager:info("Finished simulation~n"), Rcpt ! done end
        }
    ),

    % Start nodes without channels:
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

    netsim_clock_serv:start(), % Starts ticking
    receive
        done -> application:stop(netsim)
    end.
