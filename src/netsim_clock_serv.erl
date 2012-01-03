-module(netsim_clock_serv).
-include("include/netsim.hrl").

-behaviour(gen_server).

-export([start_link/0, start_simulation/0]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        watch = 0 :: pos_integer()
    }).

start_simulation() ->
    ok.

start_link() ->
    gen_server:start_link({local, ?NETSIM_CLOCK}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_call(Msg, _From, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    {noreply, State}.

terminate(normal, State) ->
    State.

code_change(_, _, State) ->
    {ok, State}.
