-module(netsim_clock_serv).

-include("include/netsim.hrl").

-behaviour(gen_fsm).

%% API callbacks
-export([start_link/0, start/1, node_work_complete/2]).

%% gen_fsm callbacks
-export([init/1, code_change/4, terminate/3, finalize/2,
        handle_info/3, handle_sync_event/4, handle_event/3]).

%% gen_fsm state callbacks
-export([wait_for_job/2, send_tick/2, node_ack/2]).

-record(state, {
        tick = 1 :: pos_integer(),
        nodes = [] :: [netsim_types:nodeid()], % Nodes that did not send ack
        event :: #event{},
        done = false % Whether all nodes are done with their work
    }
).

%% =============================================================================
%% API
%% =============================================================================

%% @doc Start netsim_clock_serv.
start_link() ->
    gen_fsm:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).

%% @doc Send event and start simulation.
-spec start(#event{}) -> ok.
start(#event{}=Event) ->
    gen_fsm:send_event(?NETSIM_CLOCK, Event).

%% @doc Ack from node when it completes its processing after receiving the tick
%%
%% WorkToDo defines whether node has some remaining work to be done
-spec node_work_complete(netsim_types:nodeid(), boolean()) -> ok.
node_work_complete(NodeId, WorkToDo) ->
    gen_fsm:send_event(?NETSIM_CLOCK, {node_ack, NodeId, WorkToDo}).

state() ->
    gen_fsm:sync_send_all_state_event(?NETSIM_CLOCK, state).

%% =============================================================================
%% States
%% =============================================================================

%% @doc Gets an event and sends it to all nodes. Starts ticking the counter
%% after that.
-spec wait_for_job(#event{}, #state{}) ->
    {next_state, send_tick, #state{}}.
wait_for_job(#event{}=Event, #state{tick=Tick}=State) ->
    Event1 = Event#event{tick=Tick},
    netsim_serv:send_event(Event1),

    % Reset counters:
    [netsim_serv:reset(N) || N <- netsim_sup:list_nodes()],

    {next_state, send_tick, State#state{event=Event1}, 0}.

%% @doc Just a tick for every node
%%
%% Intially we set #state.done = true, and update this value when we get ack's
%% from the nodes. If any ACK says "not done", then update #state.done to false.
send_tick(timeout, State=#state{tick=Time}) ->
    Nodes = netsim_sup:list_nodes(),
    %lager:info("Sending a tick to Nodes: ~p, tick: ~p", [Nodes, Time]),
    [netsim_serv:tick(Node, Time) || Node <- Nodes],
    {next_state, node_ack, State#state{nodes=Nodes, done=true}, 0}.

node_ack(timeout, #state{nodes=[], done=true, tick=T}=State) ->
    {next_state, finalize, State#state{tick=T+1}, 0};

node_ack(timeout, State) ->
    {next_state, node_ack, State};

node_ack({node_ack, N, true},
        State=#state{tick=Tick, nodes=[N], done=true}) ->
    %lager:info("Final deleted node ~p", [N]),
    % Send to all nodes finalize msg:
    [netsim_serv:finalize(Node) || Node <- netsim_sup:list_nodes()],
    % Send stop event to stats:
    ok = netsim_stats:send_stat(
        #stat{tick=Tick, action=stop}
    ),

    {next_state, finalize, State#state{nodes=[]}, 0};

node_ack({node_ack, N, D1}, State=#state{nodes=[N], tick=T}) ->
    %lager:info("Got answer from last node ~p, tick: ~p, done: ~p", [N, T, D1]),
    {next_state, send_tick, State#state{nodes=[], tick=T+1}, 0};

node_ack({node_ack, N, D1}, State=#state{nodes=Nodes, done=D2, tick=_T}) ->
    %lager:info("Got answer from node ~p, tick: ~p done: ~p~n", [N, _T, D2]),
    {next_state, node_ack, State#state{
            done = D1 and D2,
            nodes=lists:delete(N, Nodes)
        }
    }.

finalize(timeout, #state{tick=Tick}=State) ->
    {next_state, wait_for_job, State#state{tick=Tick+1}}.

%% =============================================================================
%% gen_fsm callbacks
%% =============================================================================

init([]) ->
    {ok, wait_for_job, #state{}}.

%% for debugging
handle_sync_event(state, _From, StateName, StateData) ->
    {reply, {StateName, StateData}, StateName, StateData}.

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

workflow_test() ->
    % Setup:
    meck:new(netsim_sup, [no_link]),
    meck:expect(netsim_sup, list_nodes, 0, []),
    meck:new(netsim_serv, [no_link]),
    meck:expect(netsim_serv, send_event, 1, ok),
    {ok, _} = start_link(),

    start(#event{action=add}),
    timer:sleep(10),

    ?assertMatch(
        {wait_for_job, #state{tick=2, event=#event{action=add}}},
        state()
    ),

    start(#event{action=del}),
    timer:sleep(10),

    ?assertMatch(
        {wait_for_job, #state{tick=3, event=#event{action=del}}},
        state()
    ),

    meck:unload(netsim_serv),
    meck:unload(netsim_sup).

-endif.
