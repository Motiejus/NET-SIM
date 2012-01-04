-module(netsim_serv).
-include("include/netsim.hrl").
-include("include/log_utils.hrl").

-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-export([start_link/3, add_link/2, send_event/1, tick/2, state/1, stop/1]).

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
    gen_server:call(NodeId, {tick, TickNr}).

state(NodeId) ->
    gen_server:call(NodeId, state).

stop(NodeId) ->
    gen_server:cast(NodeId, stop).

%% =============================================================================

init([Nodeid, Price, MaxLatency]) ->
    {ok, #state{nodeid=Nodeid, price=Price, max_latency=MaxLatency,
            queues=[], table=[]}}.

handle_cast({update_complete, NodeId},
    State=#state{pending_responses=Resp}) ->
    EmptyQueue = lists:all(fun(Q) -> length(Q) == 0 end, State#state.queues),
    if
        Resp == [NodeId] -> % Last reply, we can send clock "done"
            netsim_clock_serv:node_work_complete(NodeId, EmptyQueue);
        true -> ok
    end,
    State#state{pending_responses=lists:delete(NodeId, Resp)};

handle_cast({route, #route{action=change}, ReportCompleteTo},
    #state{table=_RouteTable0, nodeid=NodeId}=State) ->

    % Change current route
    % Propagate current route

    % @todo

    gen_server:cast(ReportCompleteTo, {update_complete, NodeId}),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({tick, Tick}, _, #state{queues=Queues, tick=Tick1}=S) ->
    case (Tick1+1) of
        Tick -> ok;
        _ -> throw({inconsistent_tick, Tick1, Tick})
    end,

    S = [{To, R} || {{_, To, _}, MT} <- Queues, {R, T} <- MT, T == 0],
    % S :: [{To :: nodeid(), Route :: #route{}}]

    Pending = [To || {To, _Route} <- S],
    [send_route(To, Route, self()) || {To, Route} <- S],

    % Update every queue head: decrease Tick
    % msg_queue() :: {link(), [{Msg :: #route{}, TimeLeft :: pos_integer()}]}.
    NewQ = [ { L, [{M,T-1}||{M,T}<-Arr,T=/=0] } || {L, Arr} <- Queues],

    {noreply, S#state{pending_responses=Pending, queues=NewQ}};

%% @doc Inserts link (into queue).
handle_call({add_link, {From0, To0, Metrics}}, _From,
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
                ({{F, T, M}, _Queue}, Acc)
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
handle_call({event, #event{action=add_resource, resource=R}}, _From,
        #state{table=RouteTable0, nodeid=NodeId, tick=Tick}=State) ->
    % Check if given resource does exist:
    case proplists:get_value(R, RouteTable0, '$undefined') of
        '$undefined' ->
            ok;
        _ ->
            throw({resource_already_exists, R})
    end,

    % Add new route into table:
    Cost = {0, 0},
    Route = {R, [{[NodeId], Cost}]},
    RouteTable1 = [Route|RouteTable0],

    % Propogate the new resource to neighbours:
    Msg = #route{nodeid=NodeId, route=Route, resource=R, action=add, time=Tick},
    State1 = send_route_msg(Msg, State#state{table=RouteTable1}),

    {reply, ok, State1};

%% @doc Delete resource.
handle_call({event, #event{action=del_resource, resource=R}}, _From,
        #state{table=RouteTable0, tick=Tick, nodeid=NodeId}=State) ->
    % Find route that is affected by del_resource resource id and
    % have to be deleted:
    Route =
        case proplists:get_value(R, RouteTable0) of
            [{[NodeId], _}]=Route0 -> Route0;
            _Route0 ->
                throw({inconsistent_route_table, {del_resource, R},
                        RouteTable0})
        end,
    % Delete route from table:
    RouteTable1 = proplists:delete(R, RouteTable0),

    % Propogate route deletion to neighbours:
    Msg =
        #route{nodeid=NodeId, route=Route, resource=R, action=del, time=Tick},
    State1 = send_route_msg(Msg, State#state{table=RouteTable1}),

    {reply, ok, State1};

handle_call({event, _Event}, _From, _State) ->
    ok;

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

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
        fun ({{_From, _To, Metrics}=Link, Queue}) ->
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
    Routes1 = proplists:get_value(Res, RouteTable0),
    % @todo

    ok.

%% @doc Changes route table when route change event arrives.
-spec change_route(#route{}, #state{}) -> #state{}.
change_route(
        #route{route=NewRoute, resource=Res},
        #state{table=RouteTable0, nodeid=NodeId, max_latency=MaxL}=State) ->
    Routes0 = proplists:get_value(Res, RouteTable0, []),
    % Get route (ExistingRoute) that will be affected by NewRoute:
    ExistingRoute = find_route(NewRoute, Routes0),
    % Delete ExistingRoute from routes table:
    Routes1 = lists:delete(ExistingRoute, Routes0),
    % Check, if new route max_latency =< global max_latency and new route
    % doesn't have a loop:
    {_, {Latency, _}} = NewRoute,
    Routes2 = 
        case ((not has_loop(NodeId, NewRoute)) and (Latency =< MaxL)) of
            true ->
                % Find a new optimal route:
                update_optimal([NewRoute | Routes1]);
            false ->
                Routes1
        end,

    RouteTable1 = [{Res, Routes2} | proplists:delete(Res, RouteTable0)],
    State1 = State#state{table=RouteTable1},
    % Send route update messages to neighbours:
    State2 = send_msg_after_update(Res, Routes2, Routes0, State1).

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
send_msg_after_update(R, [NewRoute|_], [], State) ->
    % Route for a new resource is added:
    send_route_msg(#route{resource=R, route=NewRoute, action=change}, State);

send_msg_after_update(R, [], [OldRoute|_], #state{nodeid=Id}=State) ->
    % Route is deleted:
    send_route_msg(#route{resource=R, nodeid=Id, action=del}, State);

send_msg_after_update(R, [NewR|_], [CurR|_], State) when NewR /= CurR ->
    % Best new route is changed:
    send_route_msg(#route{resource=R, route=NewR, action=change}, State);

send_msg_after_update(_, _, _, State) ->
    % Nothing happened = no msg
    State.

%% @doc Returns route that was propagated by the same node as given one, i.e.
%% route's second from the right element is equal to last element of the given.
-spec find_route(netsim_types:route(), [netsim_types:route()]) ->
    netsim_types:route() | undefined.
find_route({Path, _}, Routes) ->
    LastElement = hd(lists:reverse(Path)),

    Res = lists:filter(
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

%% =============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%send_route_sync_test_() ->
%    {foreach,
%        fun meck_setup/0,
%        fun meck_cleanup/1,
%        [
%            {"Send new route synchronously", fun meck_send_route_sync/0}
%        ]
%    }.
%
%meck_setup() ->
%    application:start(sasl),
%    application:start(netsim),
%    meck:new(netsim_serv, [passthrough]),
%    meck:expect(netsim_serv, handle_cast, fun
%            ({route, #route{action=change}, ReportCompleteTo},
%                State=#state{nodeid=NodeId}) ->
%                gen_server:cast(ReportCompleteTo, {update_complete, NodeId}),
%                {noreply, State};
%            (What, State) ->
%                meck:passthrough([What, State])
%        end),
%    ok.
%
%meck_cleanup(_) ->
%    meck:unload(netsim_serv),
%    ?mute_log(),
%    application:stop(netsim),
%    application:stop(sasl),
%    ?unmute_log().
%
%meck_send_route_sync() ->
%    ok.

sizeof_test() ->
    ?assertEqual(520, sizeof(#route{action=del})).

send_route_msg_test() ->
    Msg = #route{action=del},
    Link = {a, b, [{latency, 20}, {bandwidth, 64}]},
    Queues = [{Link, []}],

    #state{queues=Queues1} = send_route_msg(Msg, #state{queues=Queues}),

    ?assertMatch(
        [{{a, b, [_, _]}, [{Msg, 28}]}],
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
        [{{a, b, [_, _]}, [{#route{route={[b, c, d], {30, 42}}}, _}]}],
        Queues2
    ).

add_link_test() ->
    start_link(a, 10, 200),
    add_link(a, {b, a, [metrics0]}),
    add_link(a, {a, b, [metrics1]}),

    ?assertEqual(
        [{{a, b, [metrics1]}, []}],
        (state(a))#state.queues
    ).

add_resource_test() ->
    % Create 'a' and 'b' nodes:
    start_link(a, 10, 200),
    start_link(b, 20, 200),
    % Create link between them:
    add_link(a, {b, a, [{latency, 20}, {bandwidth, 64}]}),
    add_link(b, {b, a, [{latency, 20}, {bandwidth, 64}]}),

    % Add resource '1' to 'a' node:
    ok = send_event(#event{resource={a, 1}, action=add_resource}),

    ?assertEqual(
        [{{a, 1}, [{[a], {0, 0}}]}],
        (state(a))#state.table
    ),

    ?assertMatch(
        [{{a, b, _}, [_]}],
        (state(a))#state.queues
    ).

del_resource_test() ->
    % Dirty hack: reuse add_resource_test() setup.

    % Del resource '1' from 'a' node:
    ok = send_event(#event{resource={a, 1}, action=del_resource}),

    ?assertEqual(
        [],
        (state(a))#state.table
    ),

    ?assertMatch(
        [{{a, b, _}, [_, _]}],
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
    Link = {from, to, [{latency, 10}, {bandwidth, 64}]},
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
            {{d, b, [{latency, 11}, {bandwidth, 64}]}, []}
        ],
        table = [
            {{a, 1}, [{[a, e, f], {20, 3}}]}
        ]
    },

    ?assertMatch(
        [{[a, b, c], _}, {[a, e, f], _}],
        proplists:get_value({a, 1}, (change_route(Route0, State0))#state.table)
    ),

    % Delete existing route case.
    Route1 = #route{
        resource = {a, 1},
        route = {[a, n , e], {44, 1}},
        action = change
    },

    ?assertMatch(
        [],
        proplists:get_value({a, 1}, (change_route(Route1, State0))#state.table)
    ).

-endif.
