-module(netsim_serv).
-include("include/netsim.hrl").

-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-export([start_link/3, add_link/2, send_event/1, tick/2, state/1]).

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
send_event(Event=#event{nodeid=NodeId}) ->
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
    #state{table=RouteTable0, nodeid=NodeId}=State) ->

    % Change current route
    % Propagate current route

    % @todo

    gen_server:cast(ReportCompleteTo, {update_complete, NodeId}),
    {noreply, State};

handle_cast(Msg, State) ->
    {noreply, State}.

handle_call({tick, Tick}, _, #state{nodeid=NodeId, queues=Queues, tick=T}=S) ->
    case (T+1) of
        Tick -> ok;
        _ -> throw({inconsistent_tick, T, Tick})
    end,

    S = [{To, R} || {{_, To, _}, MT} <- Queues, {R, T} <- MT, T == 0],
    % S :: [{To :: nodeid(), Route :: #route{}}]


    Pending = [To || {To, _Route} <- S],
    [send_route(To, Route, self()) || {To, Route} <- S],

    % Update every queue head: decrease Tick
    % msg_queue() :: {link(), [{Msg :: #route{}, TimeLeft :: pos_integer()}]}.
    NewQ = [ { L, [{M,T-1}||{M,T}<-Arr] } || {L, Arr} <- Queues, T =/= 0],

    {noreply, S#state{pending_responses=Pending, queues=NewQ}};

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
        #state{table=RouteTable0, nodeid=NodeId, price=Price, tick=Tick,
                queues=Queues}=State) ->
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
    State1 = send_msg(Msg, State#state{table=RouteTable1}),

    {reply, ok, State1};

%% @doc Delete resource.
handle_call({event, Ev=#event{action=del_resource, resource=R}}, _From,
        #state{table=RouteTable0, tick=Tick, nodeid=NodeId,
            queues=Queues}=State) ->
    % Find route that is affected by del_resource resource id and
    % have to be deleted:
    Route =
        case proplists:get_value(R, RouteTable0) of
            [{[NodeId], _}]=Route0 -> Route0;
            Route0 ->
                throw({inconsistent_route_table, {del_resource, R},
                        RouteTable0})
        end,
    % Delete route from table:
    RouteTable1 = proplists:delete(R, RouteTable0),

    % Propogate route deletion to neighbours:
    Msg =
        #route{nodeid=NodeId, route=Route, resource=R, action=del, time=Tick},
    State1 = send_msg(Msg, State#state{table=RouteTable1}),

    {reply, ok, State1};

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

%% @doc Puts route message to all outgoing queues.
-spec send_msg(#route{}, #state{}) -> #state{}.
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

%% @doc Returns term() size in bits.
sizeof(Term) ->
    erlang:bit_size(term_to_binary(Term)).

%% @doc Changes route table.
%% Change steps:
%% 1) Check if NewRoute->nodeid is in CurrentRoute->path:
%% 1.1) True: Check if there is no loop in NewRoute->route:
%% 1.1.1) True: delete CurrentRoute, take the best route from History, send msg
%%        change to neighbours;
%% 1.1.2) False: update CurrentRoute, send change msg to neighbours;
%% 1.2) False: update route History and reelect a new best route, if new best
%% route is found, @todo loop send msg to neighbours about it;
%% @todo check if routetable0 res exists outside.
change_route(
        #route{nodeid=NeighbourNodeId, route=NewRoute, resource=Res},
        #state{table=RouteTable0, nodeid=Nodeid}=State) ->
    ResourceRoutes = proplists:get_value(Res, RouteTable0),
    [CurrentOptimalRoute|_] = ResourceRoutes,

    % get R = find_route(),
    % delete R from Routes
    % if without loop NewRoute, insert it
    % update_route

    ok.

%% @doc Returns route that was propagated by the same node as given one, i.e.
%% route's second from the right element is equal to last element of the given.
-spec find_route(netsim_types:route(), [netsim_types:route()]) ->
    netsim_types:route() | undefined.
find_route({Path, _} = Route, Routes) ->
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
    ok = send_event(#event{nodeid=a, resource={a, 1}, action=add_resource}),

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
    ok = send_event(#event{nodeid=a, resource={a, 1}, action=del_resource}),

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

-endif.
