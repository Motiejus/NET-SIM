-module(netsim_stats).
-include("include/netsim.hrl").
-include("include/log_utils.hrl").

-behaviour(gen_server).

%%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

%%% API
-export([start_link/0, modify_resource/1, send_stat/1,
        add_route/2, del_route/2, define_resource/3]).

-record(state, {
        res = update_this :: netsim_types:resource(),
        upto :: pos_integer(),
        callback :: function(),
        sofar = [{add, []}, {del, []}] %:: [{add,
%                [{netsim_types:latency(), pos_integer()}]
%            }, {del, 
%                [{netsim_types:latency(), pos_integer()}]
%            }]
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc API
modify_resource(Event=#event{}) ->
    gen_server:call(?MODULE, {modify_resource, Event}).

%% @doc API for adding and deleting route
-spec add_route(When :: netsim_types:latency(), netsim_types:resource()) -> ok.
add_route(When, Resource) ->
    gen_server:call(?MODULE, {add_del_route, add, When, Resource}).
-spec del_route(When :: netsim_types:latency(), netsim_types:resource()) -> ok.
del_route(When, Resource) ->
    gen_server:call(?MODULE, {add_del_route, del, When, Resource}).

-spec define_resource(netsim_types:resource(), integer(), function()) -> ok.
define_resource(Resource, NumNodes, Fun) ->
    gen_server:call(?MODULE, {define_resource, Resource, NumNodes, Fun}).

-spec send_stat(#event{}) -> ok.
send_stat(Event) ->
    ok.

%% @doc Define resource
handle_call({define_resource, Res, Upto, Fun}, _, State=#state{}) ->
    {reply, ok, State#state{res=Res, upto=Upto, callback=Fun}};

%% @doc Add or delete node
handle_call({modify_resource, _E}, _, State=#state{res=update_this}) ->
    {reply, tell_resource_before_collecting, State};

handle_call({modify_resource, #event{time=T, action=A, resource=Res}},
    _From, State=#state{res=Res, sofar=Sofar}) ->
    {reply, ok, State#state{sofar=set_value(A, [{T, 0}], Sofar)}};

handle_call({modify_resource, #event{}}, _From, State=#state{}) ->
    {reply, ok, State};

%% @doc Add or delete route to Resource, update statistics
%-spec handle_call({add_del_route, add | del, netsim_types:latency(),
%        netsim_types:resource()}, term(), #state{}} -> {reply, ok, #state{}}.
handle_call({add_del_route, A, T, Res}, _From,
    State=#state{res=Res, sofar=Sofar}) ->

    NewAcc = case proplists:get_value(A, Sofar) of
        [{T, HowMuchSoFar}|Old] -> [{T, HowMuchSoFar+1}|Old];
        X = [{_, HowMuchSoFar}|_] -> [{T, HowMuchSoFar+1}|X]
    end,

    {_, LastEntered} = hd(NewAcc),
    UpTo = State#state.upto,
    case LastEntered of
         UpTo ->
             (State#state.callback)(NewAcc);
         _ when LastEntered > State#state.upto ->
            lager:error("Too many nodes: ~p", [NewAcc]);
        _ -> ok
    end,
    {reply, ok, State#state{sofar=set_value(A, NewAcc, Sofar)}};

handle_call({add_del_route, _, _, _}, _From, State) ->
    {reply, ok, State}.

init([]) ->
    {ok, #state{}}.
handle_cast(undefined, state) ->
    undefined.
terminate(normal, _State) ->
    ok.
code_change(_, _, State) ->
    {ok, State}.
handle_info(_Msg, State) ->
    {noreply, State}.

set_value(K, V, Proplist) ->
    [{K, V}|proplists:delete(K, Proplist)].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stats_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_add_resource_before/0,
            fun test_functional/0
        ]
    }.

test_add_resource_before() ->
    ?assertEqual(
        tell_resource_before_collecting,
        modify_resource(#event{resource=x})
    ).

test_functional() ->
    Rcpt = self(),
    define_resource({a,1}, 5, fun(Arr) -> Rcpt ! Arr end), % 5 nodes in the graph
    modify_resource(#event{time=3, action=add, resource={a,1}}),
    add_route(3, {a,1}),
    add_route(5, {a,1}),
    add_route(5, {a,1}),
    add_route(6, {a,1}),
    add_route(7, {a,1}),
    ?assertEqual([{7,5},{6,4},{5,3},{3,1}],
        receive A -> A end),
    modify_resource(#event{time=20, action=del, resource={a,1}}),
    del_route(21, {a,1}),
    del_route(21, {b,2}), % garbage
    del_route(22, {a,1}),
    del_route(23, {a,1}),
    del_route(30, {a,1}),
    del_route(29, {c,2}), % garbage
    del_route(30, {a,1}),
    ?assertEqual([{30,5}, {23,3}, {22,2}, {21,1}, {20,0}],
        receive A -> A end).


setup() ->
    ?mute_log(),
    ok = application:start(lager),
    ?unmute_log(),
    start_link().

cleanup(_) ->
    ?mute_log(),
    application:stop(lager),
    ?unmute_log().

-endif.
