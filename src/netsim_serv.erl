-module(netsim_serv).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        queue :: netsim_types:msg_queue(),
        nodeid :: netsim_types:nodeid(),
        table :: netsim_types:route_table()
    }).

start_link(Nodeid) ->
    gen_server:start_link({local, Nodeid}, ?MODULE, [Nodeid], []).

init([Nodeid]) ->
    io:format("~p~n", [Nodeid]),
    {ok, #state{nodeid=Nodeid, queue=[], table=[]}}.

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
