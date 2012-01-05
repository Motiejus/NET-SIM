-module(netsim_clock_serv).
-include("include/netsim.hrl").
-include("include/log_utils.hrl").

-behaviour(gen_fsm).

%% API callbacks
-export([start_link/0, node_work_complete/2, send_data_file/1, start/0,
        sync_state/1]).

%% gen_fsm callbacks
-export([init/1, code_change/4, terminate/3, finalize/2,
        handle_info/3, handle_sync_event/4, handle_event/3]).

%% gen_fsm state callbacks
-export([wait_for_data/2, send_tick/2, node_ack/2]).

-record(state, {
        time = 1 :: pos_integer(),
        data = [] :: [#'event'{}],
        nodes = [] :: [netsim_types:nodeid()], % Nodes that did not send ack
        done = false % Whether all nodes are done with their work
    }
).

%% API
%% =============================================================================
send_data_file(Simulation) ->
    gen_fsm:send_event(?NETSIM_CLOCK, {data_file, Simulation}).

start_link() ->
    gen_fsm:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).

start() ->
    gen_fsm:send_event(?NETSIM_CLOCK, timeout).

%% @doc Ack from node when it completes its processing after receiving the tick
%%
%% WorkToDo defines whether node has some remaining work to be done
-spec node_work_complete(netsim_types:nodeid(), boolean()) -> ok.
node_work_complete(NodeId, WorkToDo) ->
    gen_fsm:send_event(?NETSIM_CLOCK, {node_ack, NodeId, WorkToDo}).

%% Ticking implementation
%% =============================================================================
-spec wait_for_data([#event{}], #state{}) -> {next_state,send_tick,#state{},0}.
wait_for_data({data_file, Data}, State=#state{}) ->
    {next_state, send_tick, State#state{data=Data}}. % Waiting for a trigger

%% @doc Have a message to send. Flush messages
%%
%% That are supposed to be flushed during this tick
-spec send_tick(tick, #state{}) -> {next_state, send_tick, #state{}, 0}.
send_tick(timeout, S=#state{time=W, data=[E=#event{time=T}|Evs]}) when W == T ->
    netsim_serv:send_event(E),
    {next_state, send_tick, S#state{data=Evs}, 0};

%% @doc Just a tick for every node
%%
%% Intially we set #state.done = true, and update this value when we get ack's
%% from the nodes. If any ACK says "not done", then update #state.done to false.
send_tick(timeout, State=#state{time=Time}) ->
    Nodes = netsim_sup:list_nodes(),
    [netsim_serv:tick(Node, Time) || Node <- Nodes],
    {next_state, node_ack,
        State#state{time=Time+1, nodes=Nodes, done=true}}.

node_ack({node_ack, N, true}, State=#state{nodes=[N], done=true, data=[]}) ->
    {next_state, finalize, State#state{nodes=[]}};

node_ack({node_ack, N, _}, State=#state{nodes=[N]}) ->
    {next_state, send_tick, State#state{nodes=[]}, 0};

node_ack({node_ack, N, D1}, State=#state{nodes=Nodes, done=D2}) ->
    case lists:member(N, Nodes) of
        true ->
            {next_state, node_ack, State#state{
                    done = D1 and D2,
                    nodes=lists:delete(N, Nodes)
                }};
        false ->
            throw({node_already_deleted, N})
    end.

finalize(timeout, State) ->
    {next_state, finalize, State};
finalize(stop, _State) ->
    {stop, stopped}.

%% gen_fsm callbacks
%% =============================================================================
init([]) ->
    {ok, wait_for_data, #state{}}.

%% for debugging
handle_sync_event(give_me_state_name, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData, 0}.
handle_event(event, statename, State) ->
    {stop, undefined, State}.
handle_info(info, statename, State) ->
    {stop, undefined, State}.
terminate(_, _, _) ->
    ok.
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

sync_state(State) ->
    case gen_fsm:sync_send_all_state_event(?NETSIM_CLOCK, give_me_state_name) of
        State -> ok;
        _  -> timer:sleep(10), sync_state(State)
    end.

single_tick() ->
    % Copied from bootstrap
    {ok, SimulationFile} = file:consult(
        filename:join([code:priv_dir(netsim), "simulation.txt"])),
    netsim_clock_serv:send_data_file(SimulationFile),
    netsim_clock_serv:start(),
    sync_state(finalize),

    % Ensure all events were sent to the nodes
    ?assertEqual(length(SimulationFile),
        length([ok || {_,{_,send_event,_},_} <- meck:history(netsim_serv)])).

setup() ->
    application:start(sasl),
    application:start(netsim),
    meck:new([netsim_serv, netsim_sup]),
    meck:expect(netsim_sup, list_nodes, 0, [n1, n2]), % We have 2 nodes to test
    meck:expect(netsim_serv, send_event, fun(_) -> ok end),

    T = ets:new(eunit_state, [set, public]),
    ets:insert(T, {n1, false}),

    meck:expect(netsim_serv, tick,
        fun (n1, _Time) ->
                Complete = ets:lookup_element(T, n1, 2),
                ets:insert(T, {n1, true}),

                netsim_clock_serv:node_work_complete(n1, Complete);
            (n2, _Time) ->
                netsim_clock_serv:node_work_complete(n2, true)
        end).

cleanup(_) ->
    meck:unload([netsim_serv, netsim_sup]),

    ?mute_log(),
    application:stop(netsim),
    application:stop(sasl),
    ?unmute_log().

-endif.
