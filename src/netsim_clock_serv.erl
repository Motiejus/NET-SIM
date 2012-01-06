-module(netsim_clock_serv).

-include("include/netsim.hrl").
-include("include/log_utils.hrl").

-behaviour(gen_fsm).

%% API callbacks
-export([start_link/0, node_work_complete/2, initialize/1, start/0]).

%% gen_fsm callbacks
-export([init/1, code_change/4, terminate/3, finalize/2,
        handle_info/3, handle_sync_event/4, handle_event/3]).

%% gen_fsm state callbacks
-export([initialize/2, send_tick/2, node_ack/2]).

-record(state, {
        time = 1 :: pos_integer(),
        nodes = [] :: [netsim_types:nodeid()], % Nodes that did not send ack
        data = [] :: [#'event'{}],
        done = false, % Whether all nodes are done with their work
        callback
    }
).

%% API
%% =============================================================================

%% @doc Callback is function which will be called with result data on end
-spec initialize({Simulation :: [#event{}], Callback :: function()}) -> ok.
initialize(Data) ->
    gen_fsm:send_event(?NETSIM_CLOCK, Data).

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


%% @doc Initialize clock. Give it a list of events and a completion callback
%%
%% Callback will be called when the simulation completes with these arguments:
%% 
-spec initialize({[#event{}], function()}, #state{}) ->
    {next_state, send_tick, #state{}}.
initialize({Data, Callback}, State=#state{}) ->
    {next_state, send_tick, State#state{data=Data, callback=Callback}}.

%% @doc Have a message to send. Flush messages
%%
%% That are supposed to be flushed during this tick
-spec send_tick(tick, #state{}) -> {next_state, send_tick, #state{}, 0}.
send_tick(timeout, S=#state{time=W, data=[E=#event{time=T}|Evs]}) when W == T ->
    %lager:info("Sending event: ~p~p", [E, {time, W}]),
    netsim_serv:send_event(E),
    %{next_state, send_tick, S#state{data=Evs}, 0};
    send_tick(timeout, S#state{data=Evs});

%% @doc Just a tick for every node
%%
%% Intially we set #state.done = true, and update this value when we get ack's
%% from the nodes. If any ACK says "not done", then update #state.done to false.
send_tick(timeout, State=#state{time=Time}) ->
    Nodes = netsim_sup:list_nodes(),
    %lager:info("Sending a tick to Nodes: ~p, tick: ~p", [Nodes, Time]),
    [netsim_serv:tick(Node, Time) || Node <- Nodes],
    {next_state, node_ack, State#state{nodes=Nodes, done=true}}.

node_ack({node_ack, N, true}, State=#state{nodes=[N], done=true, data=[]}) ->
    %lager:info("Final deleted node ~p", [N]),
    {next_state, finalize, State#state{nodes=[]}, 0};

node_ack({node_ack, N, _}, State=#state{nodes=[N], time=T}) ->
    %lager:info("Got answer from last node ~p, time: ~p", [N, T]),
    {next_state, send_tick, State#state{nodes=[], time=T+1}, 0};

node_ack({node_ack, N, D1}, State=#state{nodes=Nodes, done=D2, time=T}) ->
    %lager:info("Got answer from node ~p, time: ~p", [N, T]),
    {next_state, node_ack, State#state{
            done = D1 and D2,
            nodes=lists:delete(N, Nodes)
        }
    }.

finalize(timeout, State=#state{callback=Callback}) ->
    Callback(yadda),
    {next_state, finalize, State}.

%% gen_fsm callbacks
%% =============================================================================
init([]) ->
    {ok, initialize, #state{}}.

%% for debugging
handle_sync_event(give_me_state_name, _From, StateName, StateData) ->
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
    {ok, SimulationFile} = file:consult(
        filename:join([code:priv_dir(netsim), "simulation.txt"])),
    Rcpt = self(),
    netsim_clock_serv:initialize({SimulationFile, fun(Res) -> Rcpt ! Res end}),
    netsim_clock_serv:start(),
    receive
        A -> A
    end,
    %lager:info("Received ~p", [A]),
    
    % @todo Replace with a fully deterministic thing
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
