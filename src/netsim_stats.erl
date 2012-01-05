-module(netsim_stats).
-include("include/netsim.hrl").
-include("include/log_utils.hrl").

-behaviour(gen_server).

%%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

%%% API
-export([start_link/0, add_del_node/1, add_route/2, del_route/2,
        define_resource/2]).

-record(state, {
        res = update_this :: netsim_types:resource(),
        waiting_for = add_resource :: netsim_types:action(),
        upto :: pos_integer(),
        sofar = [{add, []}, {del, []}] %:: [{add,
%                [{netsim_types:latency(), pos_integer()}]
%            }, {del, 
%                [{netsim_types:latency(), pos_integer()}]
%            }]
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc API
add_del_node(Event=#event{}) ->
    gen_server:call(?MODULE, {add_del_node, Event}).

%% @doc API for adding and deleting route
-spec add_route(When :: netsim_types:latency(), netsim_types:resource()) -> ok.
add_route(When, Resource) ->
    gen_server:call({add_del_route, add, When, Resource}).
-spec del_route(When :: netsim_types:latency(), netsim_types:resource()) -> ok.
del_route(When, Resource) ->
    gen_server:call({add_del_route, del, When, Resource}).

-spec define_resource(netsim_types:resource(), integer()) -> ok.
define_resource(Resource, Upto) ->
    gen_server:call({define_resource, Resource, Upto}).


%% @doc Define resource
handle_call({define_resource, Res, Upto}, _, State=#state{}) ->
    {reply, ok, State#state{res=Res, upto=Upto}};

%% @doc Add or delete node
handle_call({add_del_node, _}, _, State=#state{res=update_this}) ->
    {reply, tell_resource_before_collecting, State};

handle_call({add_del_node, #event{time=T, action=A, resource=Res}}, _From,
    State=#state{res=Res, waiting_for=A, sofar=Sofar}) ->
    {reply, ok, State#state{sofar=set_value(A, [{T, 0}], Sofar)}};

handle_call({add_del_node, #event{}}, _From, State=#state{}) ->
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
            lager:info("All complete: ~p", [NewAcc]);
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
        fun start_link/0,
        fun cleanup/1,
        [
            fun test_add_node_before/0,
            fun test_functional/0
        ]
    }.

test_add_node_before() ->
    ?assertEqual(
        tell_resource_before_collecting,
        add_del_node(#event{resource=x})
    ).

test_functional() ->
    ok.

cleanup(_) -> ok.

-endif.
