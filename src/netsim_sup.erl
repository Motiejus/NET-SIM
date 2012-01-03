
-module(netsim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_node/2, list_nodes/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Module, Type, Args), {I, {Module, start_link, Args}, permanent,
                                5000, Type, [I]}).

%% =============================================================================
%% API functions
%% =============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec list_nodes() -> [netsim_types:nodeid()].
list_nodes() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

-spec add_node(netsim_types:nodeid(), netsim_types:cost()) -> ok.
add_node(NodeId, Price) ->
    {ok, _} =
        supervisor:start_child(?MODULE, ?CHILD(NodeId, netsim_serv, worker,
            [NodeId, Price])).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
            [
                ?CHILD(netsim_clock_serv, netsim_clock_serv, worker, [])
            ]
        }
    }.
