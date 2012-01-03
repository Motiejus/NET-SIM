-module(netsim_clock_serv).
-include("include/netsim.hrl").

-behaviour(gen_server).

-export([start_link/0, start_simulation/0, send_simulation_file/1]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        watch = 0 :: pos_integer(),
        simulation = [] :: [netsim_types:simulation_event()]
    }
).

send_simulation_file(Simulation) ->
    gen_server:cast(?NETSIM_CLOCK, {send_simulation, Simulation}).

start_simulation() ->
    ok.

start_link() ->
    gen_server:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_cast({send_simulation, Simulation}, State) ->
    {noreply, State#state{simulation=Simulation}};

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
