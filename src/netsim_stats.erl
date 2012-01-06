-module(netsim_stats).

-include("include/netsim.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, send_stat/1, define_event/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
    event :: #stat{},
    log  = [] :: list(),
    nodes = []:: [netsim_types:nodeid()] % list of nodes for waiting events from
    %% nodes
}).

%% =============================================================================

%% @doc Start stats process.
start_link() ->
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

%% =============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(state, _, State) ->
    {reply, State, State};

%% @doc Define final event and start logging.
handle_call({define, #stat{nodeid=NodeId}=Event}, _From, State) ->
    lager:info("netsim_stats: define event"),

    State1 = State#state{ 
        nodes = lists:delete(NodeId, netsim_sup:list_nodes()),
        event = Event
    },

    {reply, ok, State1};

%% @doc Stop event.
handle_call({event, #stat{action=stop, tick=Tick}=Ev}, _, State) -> 
    lager:info("~p: converged.~n", [Tick]),

    {reply, ok, State#state{log=[Ev|State#state.log]}};

%% @doc Receive last missing and matching event.
handle_call(
    {event, #stat{nodeid=NodeId, action=Action, resource=Res, tick=Tick}=Ev}, _,
    #state{nodes=[NodeId], event=#stat{action=Action, resource=Res}}=State) ->
    lager:info("~p: last event: ~p, tick_log: ~p",
        [Tick, Ev, proplists:get_value(tick, State#state.log)]),

    {reply, ok, State#state{nodes=[], log=[Ev|State#state.log]}};

%% @doc Receive matching event.
handle_call({event,
        #stat{nodeid=NodeId, action=Action, resource=Res, tick=Tick}}, _,
        #state{nodes=Nodes, event=#stat{action=Action, resource=Res},
                log=Log}=State) ->
    %lager:info("~p: matching event: ~p, nodes_left: ~p", [Tick, Ev, Nodes]),

    % Update tick log:
    TickLog0 = proplists:get_value(tick, Log, []),
    Tick0 = proplists:get_value(Tick, TickLog0, 0),
    Log1 = [
        {tick, [{Tick, Tick0+1}|proplists:delete(Tick, TickLog0)]} |
        proplists:delete(tick, Log)
    ],

    {reply, ok, State#state{nodes=lists:delete(NodeId, Nodes), log=Log1}};

handle_call({event, 
        #stat{nodeid=NodeId, action=stats, tick=Tick, tx=TX, rx=RX}=Ev},
        _, State) ->
    lager:info("~p: nodeid: ~p, tx: ~p, rx: ~p", [Tick, NodeId, TX, RX]),

    {reply, ok, State#state{log=[Ev|State#state.log]}};

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
    ok = send_stat(#stat{action=del, resource={a,1}, nodeid=b}),
    ok = send_stat(#stat{action=del, resource={a,1}, nodeid=a, tick=69}),

    ?assertMatch(
        [#stat{action=del, resource={a, 1}, nodeid=a, tick=69}],
        (state())#state.log
    ),

    ok = send_stat(#stat{action=stop, tick=71}),
    ?assertMatch(
        [_, _],
        (state())#state.log
    ),

    % Cleanup:
    meck:unload(netsim_sup).

-endif.
