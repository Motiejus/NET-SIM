-module(netsim_stats).

-include("include/netsim.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, send_stat/1, define_event/1, state/0, log/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
    event :: #stat{},
    log  = #log{} :: #log{},
    nodes = []:: [netsim_types:nodeid()] % list of nodes for waiting events from
    %% nodes
}).

%% =============================================================================

%% @doc Start stats process.
start_link() ->
    pg2:create(?NETSIM_PUBSUB),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Sends stats event.
-spec send_stat(#stat{}) -> ok.
send_stat(Event) ->
    gen_server:call(?MODULE, {event, Event}).

%% @doc Defines when to log time (i.e. what events should be received from all
%% nodes):
define_event(#stat{}=Event) ->
    gen_server:call(?MODULE, {define, Event}).

state() ->
    gen_server:call(?MODULE, state).

log() ->
    (state())#state.log.

%% =============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(state, _, State) ->
    {reply, State, State};

%% @doc Define final event and start logging.
handle_call({define, #stat{}=Event}, _From, State) ->
    State1 = State#state{ 
        nodes = netsim_sup:list_nodes(),
        event = Event
    },

    {reply, ok, State1};

%% @doc Stop event.
handle_call({event, #stat{action=stop, tick=Tick}=Ev}, _, State) -> 
    lager:info("~p: converged.~n", [Tick]),

    {reply, ok, update_event_log(Ev, State)};

%% @doc Receive last missing and matching event.
handle_call(
    {event, #stat{nodeid=NodeId, action=Action, resource=Res, tick=Tick}=Ev}, _,
    #state{nodes=[NodeId], event=#stat{action=Action, resource=Res}}=State) ->
    lager:info("~p: last event: ~p", [Tick, Ev]),

    % Update tick counter:
    State1 = update_tick_log(Tick, State),
    State2 = update_event_log(Ev, State1),

    % Wait for traffic info:
    {reply, ok,
        State2#state{event=#stat{action=done}, nodes=netsim_sup:list_nodes()}};

%% @doc Receive matching event.
handle_call({event,
        #stat{nodeid=NodeId, action=Action, resource=Res, tick=Tick}}, _,
        #state{nodes=Nodes, event=#stat{action=Action, resource=Res}}=State) ->
    %lager:info("~p: matching event: ~p, nodes_left: ~p", [Tick, Ev, Nodes]),

    % Update tick log:
    State1 = update_tick_log(Tick, State),

    {reply, ok, State1#state{nodes=lists:delete(NodeId, Nodes)}};

handle_call({event, 
        #stat{nodeid=NodeId, action=traffic}=Ev},
        _, #state{nodes=Nodes}=State) ->
    % Delete node from nodes list and update traffic log:
    State1 = update_traffic_log(
        Ev,
        State#state{nodes=lists:delete(NodeId, Nodes)}
    ),

    % Send 'finished' msg if all nodes sent traffic stats:
    if 
        State1#state.nodes == [] ->
            [Pid ! finished || Pid <- pg2:get_members(?NETSIM_PUBSUB)];
        true ->
            ok
    end,

    {reply, ok, State1};

handle_call({event, _Ev}, _, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

%% =============================================================================
%% Helpers
%% =============================================================================

update_traffic_log(#stat{nodeid=NodeId, tx=TX, rx=RX},
        #state{log=Log}=State) ->
    Log1 = Log#log{traffic=[{NodeId, TX+RX}|Log#log.traffic]},

    State#state{log=Log1}.

update_event_log(#stat{}=Ev, #state{log=Log}=State) ->
    Log1 = Log#log{events=[Ev|Log#log.events]},

    State#state{log=Log1}.

update_tick_log(Tick, #state{log=Log}=State) ->
    Ticks = Log#log.ticks,
    TickCount = proplists:get_value(Tick, Ticks, 0),
    Log1 = Log#log{
        ticks = [{Tick, TickCount+1}|proplists:delete(Tick, Ticks)]
    },

    State#state{log=Log1}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_traffic_log_test() ->
    State0 = update_traffic_log(#stat{nodeid=foobar, tx=1, rx=2}, #state{}),
    State1 = update_traffic_log(#stat{nodeid=qwerty, tx=2, rx=5}, State0),
    
    ?assertEqual(
        [{qwerty, 7}, {foobar, 3}],
        State1#state.log#log.traffic
    ).

update_event_log_test() ->
    State0 = update_event_log(#stat{nodeid=foobar, action=random}, #state{}),
    State1 = update_event_log(#stat{nodeid=qwerty, action=del}, State0),

    ?assertMatch(
        [#stat{nodeid=qwerty}, #stat{nodeid=foobar}],
        State1#state.log#log.events
    ).

update_tick_log_test() ->
    State0 = update_tick_log(10, #state{}),
    State1 = update_tick_log(10, State0),

    ?assertEqual(
        [{10, 2}],
        State1#state.log#log.ticks
    ).

workflow_test() ->
    % Setup:
    meck:new(netsim_sup, [no_link]),
    meck:expect(netsim_sup, list_nodes, 0, [a, b]), 

    {ok, _} = start_link(),
    define_event(#stat{action=del, resource={a,1}}),
    ?assertMatch(
        #state{
            nodes = [a, b],
            event = #stat{action=del, resource={a,1}}
        },
        state()
    ),

    ok = send_stat(#stat{action=del, resource={x,2}}),
    ok = send_stat(#stat{action=del, resource={a,1}, nodeid=b, tick=2}),
    ok = send_stat(#stat{action=del, resource={a,1}, nodeid=a, tick=69}),
    ok = send_stat(#stat{action=traffic, nodeid=a, tx=2, rx=5}),

    ?assertMatch(
        #log{
            ticks = [{69, 1}, {2, 1}],
            events = [#stat{action=del, tick=69}],
            traffic = [{a, 7}]
        },
        (state())#state.log
    ),

    ok = send_stat(#stat{action=stop, tick=71}),
    ?assertMatch(
        [_, _],
        (state())#state.log#log.events
    ),

    % Cleanup:
    meck:unload(netsim_sup).

-endif.
