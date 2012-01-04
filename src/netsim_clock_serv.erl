-module(netsim_clock_serv).
-include("include/netsim.hrl").

-behaviour(gen_fsm).

%% API callbacks
-export([start_link/0, start_simulation/0, node_work_complete/2, send_data_file/1]).

%% gen_fsm callbacks
-export([init/1, code_change/4, terminate/3,
        handle_info/3, handle_sync_event/4, handle_event/3]).

%% gen_fsm state callbacks
-export([wait_for_data/2, send_tick/2, node_ack/2]).

-record(state, {
        time = 0 :: pos_integer(),
        data = [] :: [#'event'{}],
        nodes = [] :: [atom()], % Atoms of nodes that did not send ack
        work_left = false % Whether all nodes have work to do left
    }
).

%% API
%% =============================================================================
send_data_file(Simulation) ->
    gen_fsm:send_event(?NETSIM_CLOCK, {data_file, Simulation}).

start_simulation() ->
    ok.

start_link() ->
    gen_fsm:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).

%% @doc Ack from node when it completes its processing after receiving the tick
%%
%% WorkToDo defines whether node has some remaining work to be done
-spec node_work_complete(netsim_types:nodeid(), boolean()) -> ok.
node_work_complete(NodeId, WorkToDo) ->
    gen_fsm:send_event(?NETSIM_CLOCK, {node_ack, NodeId, WorkToDo}).


%% Ticking implementation
%% =============================================================================
-spec wait_for_data([#event{}], #state{}) -> {next_state, send_tick, #state{}}.
wait_for_data({data_file, Data}, State=#state{}) ->
    gen_fsm:send_event(?NETSIM_CLOCK, tick),
    {next_state, send_tick, State#state{data=Data}}.

%% @doc Have a message to send. Flush messages
%%
%% That are supposed to be flushed during this tick
-spec send_tick(tick, #state{}) -> {next_state, send_tick, #state{}}.
send_tick(tick, S=#state{time=W, data=[E=#event{time=T}|Evs]}) when W == T ->
    netsim_serv:send_event(E),
    {next_state, send_tick, S#state{data=Evs}};

%% @doc Just a tick for every node
send_tick(tick, State=#state{time=Time}) ->
    % enqueue another tick for future
    gen_fsm:send_event(self(), tick),

    % Enqueue a tick to all nodes
    Nodes = netsim_sup:list_nodes(),
    [netsim_serv:tick(Node, Time) || Node <- Nodes],
    {next_state, node_ack,
        State#state{time=Time+1, nodes=Nodes, work_left=false}}.

node_ack({node_ack, N, false}, State=#state{nodes=[N], work_left=false}) ->
    {next_state, send_tick, State#state{nodes=[]}};

node_ack({node_ack, N, _}, State=#state{nodes=[N]}) ->
    {next_state, send_tick, State#state{nodes=[]}};

node_ack({node_ack, N, W1}, State=#state{nodes=Nodes, work_left=W2}) ->
    case lists:member(N, Nodes) of
        true ->
            {next_state, node_ack, State#state{
                    nodes=lists:delete(N, Nodes),
                    work_left = W1 or W2
                }};
        false ->
            throw({node_already_deleted, N})
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Tick
clock_serv_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"Single tick test", fun single_tick/0}
        ]
    }.

single_tick() ->
    % Copied from bootstrap
    %{ok, SimulationFile} = file:consult(filename:join([
    %            code:priv_dir(netsim), "simulation.txt"])),
    %netsim_clock_serv:send_data_file(SimulationFile),
    ok.

setup() ->
    application:start(sasl),
    application:start(netsim),
    meck:new([netsim_serv, netsim_sup]),
    meck:expect(netsim_sup, list_nodes, 0, [n1, n2]), % We have 2 nodes to test
    meck:expect(netsim_serv, send_event, fun(_) -> ok end),

    T = ets:new(eunit_state, [set, public]),
    ets:insert(T, {n1, false}),

    meck:expect(netsim_serv, tick,
        fun(Node, Time) ->
                [{n1, N1}] = ets:lookup_element(T, n1),
                ets:insert(T, {n1, true}),
                netsim_clock_serv:node_work_complete(n1, N1),
                netsim_clock_serv:node_work_complete(n2, true)
        end).

cleanup(_) ->
    meck:unload([netsim_serv, netsim_sup]),

    error_logger:tty(false),
    application:stop(netsim),
    application:stop(sasl),
    error_logger:tty(true).

-endif.
