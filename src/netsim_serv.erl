-module(netsim_serv).

-include("include/netsim.hrl").
-include("include/log_utils.hrl").

-include_lib("eunit/include/eunit.hrl"). % @todo remove after testing

-behaviour(gen_server).

-export([start_link/3, add_link/2, send_event/1, tick/2, finalize/1,
        state/1, stop/1]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        queues = [] :: [netsim_types:msg_queue()],
        nodeid :: netsim_types:nodeid(),
        table :: netsim_types:route_table(),
        price :: netsim_types:price(),
        tick = 0 :: non_neg_integer(), % current tick
        max_latency :: pos_integer(), % max acceptable latency
        pending_responses = [] :: [netsim_types:nodeid()]
    }).

%% =============================================================================

start_link(Nodeid, Price, MaxLatency) ->
    gen_server:start_link({local, Nodeid}, ?MODULE,
        [Nodeid, Price, MaxLatency], []).

-spec add_link(netsim_types:nodeid(), netsim_types:link()) -> ok.
add_link(NodeId, Link) ->
    gen_server:call(NodeId, {add_link, Link}).

%% @doc Sends event to a node.
-spec send_event(#'event'{}) -> ok.
send_event(Event=#event{resource={NodeId, _}}) ->
    gen_server:call(NodeId, {event, Event}).

%% @doc Sends route (add/del) event.
%%
%% After locally work is done, cast {update_complete, NodeId} to ReplySuccessTo
%%
send_route(NodeId, #route{}=Route, ReplySuccessTo) ->
    gen_server:cast(NodeId, {route, Route, ReplySuccessTo}).

%% @doc Sends tick to a node.
-spec tick(netsim_types:nodeid(), pos_integer()) -> ok.
tick(NodeId, TickNr) ->
    gen_server:cast(NodeId, {tick, TickNr}).

finalize(NodeId) ->
    gen_server:call(NodeId, finalize).

state(NodeId) ->
    gen_server:call(NodeId, state).

stop(NodeId) ->
    gen_server:cast(NodeId, stop).

%% =============================================================================

init([Nodeid, Price, MaxLatency]) ->
    {ok, #state{nodeid=Nodeid, price=Price, max_latency=MaxLatency,
            queues=[], table=[]}}.

handle_cast({update_complete, NodeId}, State=#state{pending_responses=Resp,
        nodeid=Caller}) ->
    if
        Resp == [NodeId] -> % Last reply, we can send clock "done"
            netsim_clock_serv:node_work_complete(Caller, false);
        true -> ok
    end,

    {noreply, State#state{pending_responses=lists:delete(NodeId, Resp)}};

handle_cast(
    {route, #route{action=Action, resource=Res, nodeid=RemoteNodeId}=RouteMsg,
    ReportCompleteTo}, #state{nodeid=NodeId, tick=Tick, queues=Queues}=State) ->

    % Send stats event:
    ok = netsim_stats:send_stat(
        #stat{nodeid=NodeId, tick=Tick, resource=Res, action=Action}
    ),

    % Update RX:
    Queues1 = lists:map(
        fun
            ({{From, To, Metrics, {TX, RX}}, Q}) when To == RemoteNodeId ->
                {{From, To, Metrics, {TX, RX+sizeof(RouteMsg)}}, Q};
            (Q) ->
                Q
        end,
        Queues
    ),
    State1 = State#state{queues=Queues1},

    State2 = case Action of
        change -> change_route(RouteMsg, State1);
        del -> delete_route(RouteMsg, State1)
    end,

    gen_server:cast(ReportCompleteTo, {update_complete, NodeId}),
    {noreply, State2};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({tick, Tick},
        State=#state{nodeid=NodeId, queues=Queues, tick=Tick1}) ->
    case (Tick1+1) of
        Tick -> ok;
        _ -> throw({inconsistent_tick, Tick1, Tick, State})
    end,

    %lager:info("~p: node (~p) received tick.~n", [Tick, NodeId]),

    % Collect messages to send:
    S = [{To, R} || {{_, To, _, _}, MT} <- Queues, {R, T} <- MT, T == 1],
    % S :: [{To :: nodeid(), Route :: #route{}}]

    % Check if work is complete (no pending acks and queue is empty):
    Pending = [To || {To, _Route} <- S],
    EmptyQueue =
        lists:all(
            fun({_, Q}) -> length(Q) == 0 end,
            Queues
        ),

    case Pending of
        [] -> 
            netsim_clock_serv:node_work_complete(NodeId, EmptyQueue);
        _ ->
            ok
    end,

    [send_route(To, Route, NodeId) || {To, Route} <- S],

    % Update every queue head: decrease Tick and increase TX if msg is sent
    % msg_queue() :: {link(), [{Msg :: #route{}, TimeLeft :: pos_integer()}]}.
    NewQ =
        lists:map(
            fun ({{From, To, Metrics, {TX, RX}}, Q}) ->
                case Q of
                    % TimeToSend will be == 0 : delete message, inc TX
                    [{M, 1}|T] ->
                        {{From, To, Metrics, {TX+sizeof(M), RX}}, T};
                    % TimeToSend /= : dec TimeToSend
                    [{M, Time}|T] ->
                        {{From, To, Metrics, {TX, RX}}, [{M, Time-1}|T]};
                    _ ->
                        {{From, To, Metrics, {TX, RX}}, Q}
                end
            end,
            Queues
        ),

    %lager:info("~p: node (~p) finished tick.~n", [Tick, NodeId]),

    % Send total tx/rx to stats:
    {TotalTX, TotalRX} = total_traffic(NewQ),
    ok = netsim_stats:send_stat(
        #stat{tick=Tick, action=total_traffic, nodeid=NodeId, tx=TotalTX,
            rx=TotalRX} 
    ),

    {noreply, State#state{tick=Tick, pending_responses=Pending, queues=NewQ}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Inserts link (into queue).
handle_call({add_link, {From0, To0, Metrics}}, _From,
        #state{queues=Queues, nodeid=NodeId}=State) ->

    % From should be current process NodeId:
    {From, To} =
        case From0 of
            NodeId -> {From0, To0};
            _ -> {To0, From0}
        end,
    Link = {From, To, Metrics, {0, 0}},

    % Update queues (insert new link):
    Queues1 =
        lists:foldl(
            fun
                % New link:
                ({{F, T, M, _}, _Queue}, Acc)
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


%% @doc Send traffic info to stats.
handle_call(finalize, _, #state{tick=Tick, nodeid=NodeId, queues=Queues}=State) ->
    {TX1, RX1} = lists:foldl(
        fun ({{_, _, _, {TX0, RX0}}, _}, {TX, RX}) ->
            {TX+TX0, RX+RX0}
        end,
        {0, 0},
        Queues
    ),

    %lager:info("~p sending stats", [NodeId]),
    ok = netsim_stats:send_stat(
        #stat{nodeid=NodeId, tick=Tick, action=traffic, tx=TX1, rx=RX1}
    ),

    {reply, ok, State};

%% @doc Add new resource.
handle_call({event, #event{action=add, resource=R}}, _From,
        #state{table=RouteTable0, nodeid=NodeId, tick=Tick}=State) ->

    % Check if given resource does exist:
    case proplists:get_value(R, RouteTable0, '$undefined') of
        '$undefined' ->
            ok;
        _ ->
            throw({resource_already_exists, R})
    end,

    % Send init statistics:
    ok = netsim_stats:send_stat(
        #stat{resource=R, nodeid=NodeId, action=change, tick=Tick}
    ),

    % Add new route into table:
    Cost = {0, 0},
    Route = {[NodeId], Cost},
    RouteEntry = {R, [Route]},
    RouteTable1 = [RouteEntry | RouteTable0],

    % Propogate the new resource to neighbours:
    Msg = #route{nodeid=NodeId, route=Route, resource=R, action=change,
        time=Tick},
    State1 = send_route_msg(Msg, State#state{table=RouteTable1}),

    {reply, ok, State1};

%% @doc Delete resource.
handle_call({event, #event{action=del, resource=R}}, _From,
        #state{table=RouteTable0, tick=Tick, nodeid=NodeId}=State) ->
    %lager:info("Del resource: ~p~n", [State]),

    % Send init statistics:
    ok = netsim_stats:send_stat(
        #stat{resource=R, nodeid=NodeId, action=del, tick=Tick}
    ),

    % Find route that is affected by del resource id and
    % have to be deleted:
    Route =
        case proplists:get_value(R, RouteTable0) of
            [{[NodeId], _}]=Route0 -> Route0;
            _Route0 ->
                throw({inconsistent_route_table, {del, R},
                        RouteTable0})
        end,
    % Delete route from table:
    RouteTable1 = proplists:delete(R, RouteTable0),

    % Propogate route deletion to neighbours:
    Msg =
        #route{nodeid=NodeId, route=Route, resource=R, action=del, time=Tick},
    State1 = send_route_msg(Msg, State#state{table=RouteTable1}),

    {reply, ok, State1};

handle_call(state, _, State) ->
    {reply, State, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

%% =============================================================================

%% @doc Puts route message to all outgoing queues.
%% Updates latency and price of each route before inserting in queue.
-spec send_route_msg(#route{}, #state{}) -> #state{}.
send_route_msg(#route{action=Action, route=Route}=Msg,
        #state{price=Price, queues=Queues}=State) ->
    % Insert new queue item to each queue.
    Queues1 = lists:map(
        fun ({{_From, _To, Metrics, _Traffic}=Link, Queue}) ->
            Latency = proplists:get_value(latency, Metrics),
            Bandwidth = proplists:get_value(bandwidth, Metrics),

            % Update latency and price of route:
            Route1 =
                if
                    Action == change ->
                        {Path, {L, P}} = Route,
                        {Path, {L+Latency, P+Price}};
                    true ->
                        Route
                end,
     
            TimeToSend = Latency + (sizeof(Msg) div Bandwidth),
            Item =  {Msg#route{route=Route1}, TimeToSend},

            {Link, Queue ++ [Item]}
        end,
        Queues
    ),

    State#state{queues=Queues1}.

%% @doc Returns term() size in bits.
sizeof(Term) ->
    erlang:bit_size(term_to_binary(Term)).

%% @doc Deletes route from table.
-spec delete_route(#route{}, #state{}) -> #state{}.
delete_route(
        #route{resource=Res, nodeid=NodeId},
        #state{table=RouteTable0}=State) ->
    % Get routes for resource:
    Routes0 = proplists:get_value(Res, RouteTable0, []),
    % Find route to be deleted:
    ExistingRoute = find_route({[NodeId], ok}, Routes0),
    % Delete it:
    Routes1 = lists:delete(ExistingRoute, Routes0),
    % Find a new optimal:
    Routes2 = update_optimal(Routes1),
    % Update state:
    RouteTable1 =
        case Routes2 of
            [] ->
                proplists:delete(Res, RouteTable0);
            _ ->
                [{Res, Routes2} | proplists:delete(Res, RouteTable0)]
        end,
    State1 = State#state{table=RouteTable1},
    % Send update msg to neightbours:
    send_msg_after_update(Res, Routes2, Routes0, State1).

%% @doc Changes route table when route change event arrives.
-spec change_route(#route{}, #state{}) -> #state{}.
change_route(
        #route{route=NewRoute0, resource=Res},
        #state{table=RouteTable0, nodeid=NodeId, max_latency=MaxL}=State) ->
    {Path0, Cost0} = NewRoute0,
    NewRoute1 = {Path0 ++ [NodeId], Cost0},
    Routes0 = proplists:get_value(Res, RouteTable0, []),
    % Get route (ExistingRoute) that will be affected by NewRoute:
    ExistingRoute = find_route(NewRoute0, Routes0),
    % Delete ExistingRoute from routes table:
    Routes1 = lists:delete(ExistingRoute, Routes0),
    % Check, if new route max_latency =< global max_latency and new route
    % doesn't have a loop:
    {_, {Latency, _}} = NewRoute1,
    Routes2 = 
        case ((not has_loop(NodeId, NewRoute0)) and (Latency =< MaxL)) of
            true ->
                % Find a new optimal route:
                update_optimal([NewRoute1 | Routes1]);
            false ->
                Routes1
        end,

    RouteTable1 = [{Res, Routes2} | proplists:delete(Res, RouteTable0)],
    State1 = State#state{table=RouteTable1},
    % Send route update messages to neighbours:
    send_msg_after_update(Res, Routes2, Routes0, State1).

%% @doc Sorts routes in a way that the best route is head of the list.
-spec update_optimal([netsim_types:route()]) -> [netsim_types:route()].
update_optimal(Routes) ->
    lists:sort(
        fun ({_, {_, Price0}}, {_, {_, Price1}}) ->
            Price0 =< Price1
        end,
        Routes
    ).

%% @doc Sends route update messages to all neightbours if optimal route has
%% changed (sending is done by adding msgs to queue).
-spec send_msg_after_update(netsim_types:resource(), [netsim_types:route()],
    [netsim_types:route()], #state{}) -> #state{}.
send_msg_after_update(R, [NewRoute|_], [], #state{nodeid=NodeId}=State) ->
    % Route for a new resource is added:
    send_route_msg(
        #route{resource=R, route=NewRoute, action=change, nodeid=NodeId},
        State
    );

send_msg_after_update(R, [], [_OldRoute|_], #state{nodeid=Id}=State) ->
    % Route is deleted:
    send_route_msg(#route{resource=R, nodeid=Id, action=del}, State);

send_msg_after_update(R, [NewR|_], [CurR|_], #state{nodeid=NodeId}=State)
        when NewR /= CurR ->
    % Best new route is changed:
    send_route_msg(
        #route{resource=R, route=NewR, action=change, nodeid=NodeId},
        State
    );

send_msg_after_update(_, _, _, State) ->
    % Nothing happened = no msg
    State.

%% @doc Returns route that was propagated by the same node as given one, i.e.
%% route's second from the right element is equal to last element of the given.
-spec find_route(netsim_types:route(), [netsim_types:route()]) ->
    netsim_types:route() | undefined.
find_route({Path, _}, Routes) ->
    LastElement = hd(lists:reverse(Path)),

    Res =
        lists:filter(
            fun ({Path1, _}) ->
                if
                    % Route that consists of current nodeid:
                    length(Path1) < 2 ->
                        false;
                    % Possible route:
                    true ->
                        % Second element from the right:
                        [_|[E|_]] = lists:reverse(Path1),
                        case E of
                            LastElement ->
                                true;
                            _ ->
                                false
                        end
                end
            end,
            Routes
        ),

    case Res of
        [] -> undefined;
        [R] -> R;
        _ -> throw(inconsistent_route_table)
    end.

%% @doc Checks if given Route doesn't have a loop, i.e. there is not nodeid in
%% route's path.
-spec has_loop(netsim_types:nodeid(), netsim_types:route()) -> boolean().
has_loop(NodeId, {Path, _}=_Route) ->
    lists:member(NodeId, Path).

%% @doc Returns total TX/RX per all links.
-spec total_traffic([netsim_types:msg_queue()]) ->
    {netsim_types:bits(), netsim_types:bits()}.
total_traffic(Queues) ->
    lists:foldl(
        fun ({{_, _, _, {TX0, RX0}}, _}, {TX, RX}) ->
            {TX+TX0, RX+RX0}
        end,
        {0, 0},
        Queues
    ).

%% =============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

sizeof_test() ->
    ?assertEqual(520, sizeof(#route{action=del})).

send_route_msg_test() ->
    Msg = #route{action=del},
    Link = {a, b, [{latency, 20}, {bandwidth, 64}], {0, 0}},
    Queues = [{Link, []}],

    #state{queues=Queues1} = send_route_msg(Msg, #state{queues=Queues}),

    ?assertMatch(
        [{{a, b, [_, _], _}, [{Msg, 28}]}],
        Queues1
    ),

    Msg1 = #route{
        action = change,
        resource = {a, 1},
        route = {[b, c, d], {10, 9}},
        nodeid = d
    },
    State1 = #state{
        queues = Queues,
        price = 33,
        nodeid = d
    },
    #state{queues=Queues2} = send_route_msg(Msg1, State1),
    ?assertMatch(
        [{{a, b, [_, _], _}, [{#route{route={[b, c, d], {30, 42}}}, _}]}],
        Queues2
    ).

add_link_test() ->
    start_link(a, 10, 200),
    add_link(a, {b, a, [metrics0]}),
    add_link(a, {a, b, [metrics1]}),

    ?assertEqual(
        [{{a, b, [metrics1], {0, 0}}, []}],
        (state(a))#state.queues
    ).

add_test() ->
    meck:new(netsim_stats, [no_link]),
    meck:expect(netsim_stats, send_stat, 1, ok),

    % Create 'a' and 'b' nodes:
    start_link(a, 10, 200),
    start_link(b, 20, 200),
    % Create link between them:
    add_link(a, {b, a, [{latency, 20}, {bandwidth, 64}]}),
    add_link(b, {b, a, [{latency, 20}, {bandwidth, 64}]}),

    % Add resource '1' to 'a' node:
    ok = send_event(#event{resource={a, 1}, action=add}),

    ?assertEqual(
        [{{a, 1}, [{[a], {0, 0}}]}],
        (state(a))#state.table
    ),

    ?assertMatch(
        [{{a, b, _, _}, [_]}],
        (state(a))#state.queues
    ).

del_test() ->
    % Dirty hack: reuse add_test() setup.

    % Del resource '1' from 'a' node:
    ok = send_event(#event{resource={a, 1}, action=del}),

    ?assertEqual(
        [],
        (state(a))#state.table
    ),

    ?assertMatch(
        [{{a, b, _, _}, [_, _]}],
        (state(a))#state.queues
    ).

find_route_test() ->
    Route = {[a, b], []},
    Routes = [{[c], []}, {[a, b, c], []}, {[d, e, f, c], []}],

    ?assertEqual(
        {[a, b, c], []},
        find_route(Route, Routes)
    ),
    ?assertEqual(
        undefined,
        find_route(Route, [])
    ).

has_loop_test() ->
    ?assert(has_loop(a, {[c, a, d], []})),
    ?assertNot(has_loop(a, {[c, d], []})).

update_optimal_test() ->
    Routes = [{r_1, {1, 30}}, {r_2, {1, 20}}, {r_3, {1, 20}}],
    ?assertMatch(
        [{r_2, _}, {r_3, _}, _],
        update_optimal(Routes)
    ).

send_msg_after_update_test() ->
    R = {a, 1}, % Resource
    R1 = {[a, b, d], {10, 20}}, % Route
    R2 = {[a, c, d], {10, 20}}, % Route
    Link = {from, to, [{latency, 10}, {bandwidth, 64}], {0, 0}},
    Q = [{Link, []}], % Queue
    State = #state{nodeid=d, queues=Q, price=10},
    GetMsgRoute = fun (#state{queues=[{_, [{Msg, _}]}]}) -> Msg end,
    
    % Route is deleted:
    ?assertMatch(
        #route{action=del, resource=R, nodeid=d},
        GetMsgRoute(send_msg_after_update(R, [], [R1], State))
    ),

    % Route is changed:
    ?assertMatch(
        #route{action=change, route={[a, c, d], {20, 30}}},
        GetMsgRoute(send_msg_after_update(R, [R2], [R1], State))
    ),

    % Route is untouched:
    ?assertMatch(
        [{_, []}],
        (send_msg_after_update(R, [R1], [R1], State))#state.queues
    ),

    % New route is added:
    ?assertMatch(
        #route{action=change, route={[a, c, d], {20, 30}}},
        GetMsgRoute(send_msg_after_update(R, [R2], [], State))
    ).

change_route_test() ->
    % Overwrite existing route case.
    Route0 = #route{
        resource = {a,1},
        route = {[a, b, c], {10, 2}},
        action = change
    },
    State0 = #state{
        price = 10,
        max_latency = 20,
        nodeid = d,
        queues = [
            {{d, b, [{latency, 11}, {bandwidth, 64}], {0, 0}}, []}
        ],
        table = [
            {{a, 1}, [{[a, e, f, d], {20, 3}}]}
        ]
    },

    ?assertMatch(
        [{[a, b, c, d], _}, {[a, e, f, d], _}],
        proplists:get_value({a, 1}, (change_route(Route0, State0))#state.table)
    ),

    % Delete existing route case.
    Route1 = #route{
        resource = {a, 1},
        route = {[a, n, f], {44, 1}},
        action = change
    },

    ?assertMatch(
        [],
        proplists:get_value({a, 1}, (change_route(Route1, State0))#state.table)
    ).

delete_route_test() ->
    % Choose new best route after deletion.
    Route0 = #route{
        resource = {a, 1},
        nodeid = b,
        action = del
    },
    State0 = #state{
        price = 10,
        max_latency = 20,
        nodeid = d,
        queues = [
            {{d, b, [{latency, 11}, {bandwidth, 64}], {0, 0}}, []}
        ],
        table = [
            {{a, 1}, [{[a, b, d], {20, 3}}, {[a, e, d], {11, 10}}]}
        ]
    },

    ?assertMatch(
        [{[a, e, d], _}],
        proplists:get_value({a, 1}, (delete_route(Route0, State0))#state.table)
    ),
    ?assertMatch(
        [{{d, b, _, _}, [{#route{action=change, route={[a, e, d], _}}, _}]}],
        (delete_route(Route0, State0))#state.queues
    ),

    % Delete route table entry.
    Route1 = #route{
        resource = {a, 1},
        nodeid = e,
        action = del
    },
    State1 = #state{
        price = 10,
        max_latency = 20,
        nodeid = d,
        queues = [
            {{d, b, [{latency, 11}, {bandwidth, 64}], {0, 0}}, []}
        ],
        table = [
            {{a, 1}, [{[a, e, d], {11, 10}}]}
        ]
    },

    ?assertMatch(
        undefined,
        proplists:get_value({a, 1}, (delete_route(Route1, State1))#state.table)
    ),
    ?assertMatch(
        [{{d, b, _, _}, [{#route{action=del, nodeid=d}, _}]}],
        (delete_route(Route1, State1))#state.queues
    ).

tick_test() ->
    % Setup:
    stop(a), stop(b),
    timer:sleep(10),

    {ok, _} = start_link(a, 10, 200),
    start_link(b, 11, 200),
    meck:new(netsim_clock_serv, [no_link]),
    meck:expect(netsim_clock_serv, node_work_complete, 2, ok),

    add_link(a, {b, a, [{latency, 15}, {bandwidth, 64}]}),
    add_link(b, {b, a, [{latency, 15}, {bandwidth, 64}]}),

    ?assertMatch(
        [{_, []}],
        (state(a))#state.queues
    ),

    % Add resource {a, 1} to 'a' node:
    send_event(#event{resource={a, 1}, action=add, time=0}),
    ?assertMatch(
        [{{a, b, _, _}, [{#route{action=change, route={[a], _}}, _}]}],
        (state(a))#state.queues
    ),
    % Msg time to send = 21, so we need to do 21 ticks: 
    [tick(a, Tick) || Tick <- lists:seq(1, 21)],
    ?assertMatch(
        [{{a, b, _, {416, 0}}, []}], 
        (state(a))#state.queues
    ),

    ?assertEqual(
        [{{a, 1}, [{[a, b], {15, 10}}]}],
        (state(b))#state.table
    ),
    ?assertMatch(
        [{{b, a, _, {0, 416}}, [_]}],
        (state(b))#state.queues
    ),

    ok = meck:unload(netsim_stats),
    ok = meck:unload(netsim_clock_serv).

-endif.
