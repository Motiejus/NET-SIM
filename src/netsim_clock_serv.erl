-module(netsim_clock_serv).
-include("include/netsim.hrl").

-behaviour(gen_server).

-export([start_link/0, start_simulation/0, send_simulation_file/1]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        watch = 0 :: pos_integer(),
        simulation = [] :: [#'event'{}]
    }
).

%% API
%% =============================================================================
send_simulation_file(Simulation) ->
    gen_server:cast(?NETSIM_CLOCK, {send_simulation, Simulation}).

start_simulation() ->
    ok.

start_link() ->
    gen_server:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).


%% Ticking implementation
%% =============================================================================
-spec tick(#state{}) -> {noreply, #state{}}.
%% @doc No actions left, just tick
tick(State=#state{simulation=[]}) ->
    send_tick(State);

%% @doc No messages to send to the nodes, simply tick
tick(State=#state{watch=W, simulation=[#event{time=T}|_]}) when W < T ->
    send_tick(State);

%% @doc Have a message to send. Flush messages
%%
%% that are supposed to happen during this tick
tick(State=#state{watch=Watch, simulation=[Event=#event{}|Events]}) ->
    netsim_serv:send_event(Event),
    tick(State#state{simulation=Events}).

%% gen_server callbacks
%% =============================================================================
init([]) ->
    {ok, #state{}}.

handle_cast({send_simulation, Simulation}, State) ->
    {noreply, State#state{simulation=Simulation}};

handle_cast(tick, State=#state{}) ->
    gen_server:cast(?NETSIM_CLOCK, tick),
    tick(State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, State) ->
    State.

code_change(_, _, State) ->
    {ok, State}.

%% Helpers
%% =============================================================================

%% @doc Synchronously sends simple tick to all nodes
-spec send_tick(#state{}) -> {noreply, #state{}}.
send_tick(State=#state{watch=Watch}) ->
    {noreply, State#state{watch=Watch+1}}.
