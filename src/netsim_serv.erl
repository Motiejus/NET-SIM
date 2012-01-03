-module(netsim_serv).
-include("include/netsim.hrl").

-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-export([start_link/2, add_link/2, send_event/2, state/1]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        queues :: [netsim_types:msg_queue()],
        nodeid :: netsim_types:nodeid(),
        table :: netsim_types:route_table(),
        cost :: netsim_types:cost()
    }).

%% =============================================================================

start_link(Nodeid, Cost) ->
    gen_server:start_link({local, Nodeid}, ?MODULE, [Nodeid, Cost], []).

-spec add_link(netsim_types:nodeid(), netsim_types:link()) -> ok.
add_link(NodeId, Link) ->
    gen_server:call(NodeId, {add_link, Link}).

-spec send_event(#'event'{}) -> ok.
send_event(Event=#event{nodeid=NodeId}) ->
    gen_server:call(NodeId, {event, Event}).

multicast() ->
    ok.

state(NodeId) ->
    gen_server:call(NodeId, state).

%% =============================================================================

init([Nodeid, Cost]) ->
    {ok, #state{nodeid=Nodeid, cost=Cost, queues=[], table=[]}}.

handle_cast(Msg, State) ->
    {noreply, State}.

%% @doc Inserts link (into queue).
handle_call({add_link, {From0, To0, Metrics}=Link0}, _From,
        #'state'{queues=Queues, nodeid=NodeId}=State) ->

    % From should be current process NodeId:
    {From, To} = 
        case From0 of
            NodeId -> {From0, To0};
            _ -> {To0, From0}
        end,
    Link = {From, To, Metrics},

    % Update queues (insert new link):
    Queues1 =
        lists:foldl(
            fun
                % New link:
                ({{F, T, M}, Queue}, Acc)
                        when F == From, T == To, M /= Metrics ->
                   Acc; 
                % Existing link:
                ({L, _}, Acc) when L == Link ->
                    Acc;
                % Other:
                (Q, Acc) ->
                    [Q|Acc]
            end,
            [],
            Queues
        ) ++ [{Link, []}],

    {reply, ok, State#'state'{queues=Queues1}};

handle_call({event, Ev=#event{action=add_resource}}, _From, State) ->
    ok;

handle_call({event, Ev=#event{action=del_resource}}, _From, State) ->
    ok;

handle_call({event, Event}, _From, State) ->
    ok;

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(Msg, _From, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    {noreply, State}.

terminate(normal, State) ->
    State.

code_change(_, _, State) ->
    {ok, State}.


%% =============================================================================

%% @doc Puts message to all queues.
-spec send_msg(term(), #'state'{}) -> #'state'{}.
send_msg(Msg, #'state'{queues=Queues}=State) ->
    % Insert new queue item to each queue:
    Queues1 = 
        lists:map(
            fun ({{_From, _To, Metrics}=L, Queue}) ->
                Latency = proplists:get_value(latency, Metrics),
                Bandwidth = proplists:get_value(bandwidth, Metrics),

                TimeToSend = Latency + (sizeof(Msg) div Bandwidth),
                Item =  {Msg, TimeToSend},

                {L, Queue ++ [Item]}
            end,
            Queues
        ),

    State#'state'{queues=Queues1}.

%% @doc Returns term() size in bits.
sizeof(Term) ->
    erlang:bit_size(term_to_binary(Term)).

%% =============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% @todo
sizeof_test() ->
    ?assertEqual(120, sizeof({a, b, c})).

send_msg_test() ->
    Msg = {a, b, c},
    Link = {a, b, [{latency, 20}, {bandwidth, 64}]},
    Queues = [{Link, []}],

    #'state'{queues=Queues1} = send_msg(Msg, #'state'{queues=Queues}),

    ?assertMatch(
        [{{a, b, [_, _]}, [{Msg, 21}]}],
        Queues1
    ).

add_link_test() ->
    start_link(a, 10),
    add_link(a, {b, a, [metrics0]}),

    ?assertEqual(
        [{{a, b, [metrics0]}, []}],
        (state(a))#'state'.queues
    ).

-endif.
