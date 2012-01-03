-module(netsim_serv).
-include("include/netsim.hrl").

-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-export([start_link/2, add_link/2, send_event/1, tick/2, state/1]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        queues :: [netsim_types:msg_queue()],
        nodeid :: netsim_types:nodeid(),
        table :: netsim_types:route_table(),
        price :: netsim_types:price(),
        tick = 0 :: pos_integer() % current tick
    }).

%% =============================================================================

start_link(Nodeid, Price) ->
    gen_server:start_link({local, Nodeid}, ?MODULE, [Nodeid, Price], []).

-spec add_link(netsim_types:nodeid(), netsim_types:link()) -> ok.
add_link(NodeId, Link) ->
    gen_server:call(NodeId, {add_link, Link}).

%% @doc Sends event to a node.
-spec send_event(#'event'{}) -> ok.
send_event(Event=#event{nodeid=NodeId}) ->
    gen_server:call(NodeId, {event, Event}).

%% @doc Sends tick to a node.
-spec tick(netsim_types:nodeid(), pos_integer()) -> ok.
tick(NodeId, TickNr) ->
    gen_server:call(NodeId, {tick, TickNr}).

state(NodeId) ->
    gen_server:call(NodeId, state).

%% =============================================================================

init([Nodeid, Price]) ->
    {ok, #state{nodeid=Nodeid, price=Price, queues=[], table=[]}}.

handle_cast(tick, #state{nodeid=NodeId, tick=Tick}=State) ->
    % @todo 1) process queues; 2) send back tick to clock

    {noreply, State};

handle_cast(Msg, State) ->
    {noreply, State}.

%% @doc Inserts link (into queue).
handle_call({add_link, {From0, To0, Metrics}=Link0}, _From,
        #state{queues=Queues, nodeid=NodeId}=State) ->

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

    {reply, ok, State#state{queues=Queues1}};

%% @doc Add new resource.
handle_call({event, Ev=#event{action=add_resource, resource=R}}, _From,
        #state{table=RouteTable0, nodeid=NodeId, price=Price}=State) ->
    % Check if given resource does exist:
    case ([RTEntry || {{R, _, _}, _}=RTEntry <- RouteTable0]) of
        [] ->
            ok;
        _ ->
            throw({resource_already_exists, R})
    end,

    % Add new resource:
    Cost = {0, Price},
    RouteTable1 = [{{R, [NodeId], Cost}, []} | RouteTable0],

    % Propogate the new resource to neighbours:
    % @todo

    {reply, ok, #state{table=RouteTable1}}; 

%% @doc Delete resource.
handle_call({event, Ev=#event{action=del_resource, resource=R}}, _From,
        #state{table=RouteTable0}=State) ->
    % Find routes that are affected by del_resource id and have to be deleted:
    {RoutsToBeDeleted, RouteTable1} = 
        lists:foldl(
            fun
                % Current route resource = resource to be deleted:
                ({{Res, _, _}=CR, Rs}, {DeleteAcc, TableAcc}) when Res == R ->
                    {[[CR|Rs]|DeleteAcc], TableAcc};
                ({CR, Rs}=Entry, {DeleteAcc, TableAcc}) ->
                    {DeleteAcc, [Entry|TableAcc]}
            end,
            {[], []},
            RouteTable0
        ),

    % @todo send msg to neighbours about deletion
    send_msg,
            
    {reply, ok, State#state{table=RouteTable1}};

%% @doc Updates process tick.
handle_call({tick, Tick}, _From, #state{nodeid=NodeId, tick=T}=State) ->
    case (T+1) of
        Tick ->
            gen_server:cast(self(), tick),
            {reply, ok, State#state{tick=Tick}};
        _ ->
            throw({inconsistent_tick, T, Tick})
    end;

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
-spec send_msg(term(), #state{}) -> #state{}.
send_msg(Msg, #state{queues=Queues}=State) ->
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

    State#state{queues=Queues1}.

multicast() ->
    ok.

%% @doc Returns term() size in bits.
sizeof(Term) ->
    erlang:bit_size(term_to_binary(Term)).

%% =============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

sizeof_test() ->
    ?assertEqual(120, sizeof({a, b, c})).

send_msg_test() ->
    Msg = {a, b, c},
    Link = {a, b, [{latency, 20}, {bandwidth, 64}]},
    Queues = [{Link, []}],

    #state{queues=Queues1} = send_msg(Msg, #state{queues=Queues}),

    ?assertMatch(
        [{{a, b, [_, _]}, [{Msg, 21}]}],
        Queues1
    ).

add_link_test() ->
    start_link(a, 10),
    add_link(a, {b, a, [metrics0]}),
    add_link(a, {a, b, [metrics1]}),

    ?assertEqual(
        [{{a, b, [metrics1]}, []}],
        (state(a))#state.queues
    ).

add_resource_test() ->
    % Create 'a' and 'b' nodes:
    start_link(a, 10),
    start_link(b, 20),
    % Create link between them:
    add_link(a, {b, a, [{latency, 20}, {bandwith, 64}]}),
    add_link(b, {b, a, [{latency, 20}, {bandwith, 64}]}),
    % Add resource '1' to 'a' node:
    ok = send_event(#event{nodeid=a, resource=1, action=add_resource}),

    ?assertEqual(
        [{{1, [a], {0, 10}}, []}],
        (state(a))#state.table
    ).

del_resource_test() ->
    ?assert(false).

-endif.
