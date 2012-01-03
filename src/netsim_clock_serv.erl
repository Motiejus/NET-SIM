-module(netsim_clock_serv).
-include("include/netsim.hrl").

-behaviour(gen_fsm).

%% API callbacks
-export([start_link/0, start_simulation/0, node_work_complete/1, send_data_file/1]).

%% gen_fsm callbacks
-export([init/1, code_change/4, terminate/3,
        handle_info/3, handle_sync_event/4, handle_event/3]).

%% gen_fsm state callbacks
-export([wait_for_data/2, send_tick/2, node_work_complete/2]).

-record(state, {
        time = 0 :: pos_integer(),
        data = [] :: [#'event'{}],
        nodes = [] :: [atom()] % Atoms of nodes that did not send ack
    }
).

%% API
%% =============================================================================
send_data_file(Simulation) ->
    gen_fsm:send_event(?NETSIM_CLOCK, {sim_data, Simulation}).

start_simulation() ->
    ok.

start_link() ->
    gen_server:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).

node_work_complete(NodeId) ->
    gen_server:cast(?NETSIM_CLOCK, {node_work_complete, NodeId}).


%% Ticking implementation
%% =============================================================================
-spec wait_for_data([#event{}], #state{}) -> {next_state, send_tick, #state{}}.
wait_for_data({sim_data, Data}, State=#state{}) ->
    gen_fsm:send_event(?NETSIM_CLOCK, tick),
    {next_state, send_tick, State#state{data=Data}}.

%% @doc Have a message to send. Flush messages
%%
%% That are supposed to happen during this tick
-spec send_tick(tick, #state{}) -> {next_state, send_tick, #state{}}.
send_tick(tick, S=#state{time=W, data=[E=#event{time=T}|Evs]}) when W == T ->
    netsim_serv:send_event(E),
    {next_state, send_tick, S#state{data=Evs}};

%% @doc Just a tick for every node
send_tick(tick, State=#state{time=Watch}) ->
    % enqueue another tick for future
    gen_fsm:send_event(?NETSIM_CLOCK, tick),

    % Enqueue a tick to all nodes
    Nodes = netsim_sup:list_nodes(),
    [netsim_serv:tick(Node, Watch) || Node <- Nodes],
    {next_state, node_work_complete, State#state{time=Watch+1, nodes=Nodes}}.

node_work_complete(Node, State=#state{nodes=[Node]}) ->
    {next_state, send_tick, State#state{nodes=[]}};

node_work_complete(Node, State=#state{nodes=Nodes}) ->
    case lists:member(Node, Nodes) of
        true ->
            {next_state, node_work_complete,
                State#state{nodes=lists:delete(Node, Nodes)}};
        false ->
            throw({node_already_deleted, Node})
    end.

%% gen_fsm callbacks
%% =============================================================================
init([]) ->
    {ok, wait_for_data, #state{}}.
handle_sync_event(event, from, statename, State) ->
    {stop, undefined, State}.
handle_event(event, statename, State) ->
    {stop, undefined, State}.
handle_info(info, statename, State) ->
    {stop, undefined, State}.
terminate(normal, _, State) ->
    State.
code_change(_, _, _, State) ->
    {ok, State}.
