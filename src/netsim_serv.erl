-module(netsim_serv).

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
add_link(NodeId, Channel) ->
    gen_server:call(NodeId, {add_link, Channel}).

send_event(NodeId, Event) ->
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

%% @FIXME:
handle_call({add_link, {Id, Info}=Channel}, _From,
        #'state'{queues=Channels}=State) ->

    Channels1 = 
        case proplists:get_value(Id, Channels, '$undefined') of 
            '$undefined' ->
                [Channel | Channels]; % add new link
            Info ->
                ok; % link already exists
            NewRouteInfo ->
                throw(not_unique_link_id)
        end,

    {ok, State#'state'{queues=Channels1}};

handle_call({event, {add_resource, {NodeId, ResId}}}, _From, State) ->
    ok;

handle_call({event, {del_resource, {NodeId, ResId}}}, _From, State) ->
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
            fun ({{CId, {_From, _To, Metrics}}=C, Queue}) ->
                Latency = proplists:get_value(latency, Metrics),
                Bandwith = proplists:get_value(bandwidth, Metrics),

                TimeToSend = Latency + (sizeof(Msg) / Bandwith),
                Item =  [{Msg, TimeToSend}],

                {C, Queue ++ [Item]}
            end,
            Queues
        ),

    State#'state'{queues=Queues}.

%% @doc Returns term() size in bits.
sizeof(Term) ->
    erlang:bit_size(term_to_binary(Term)).

%% =============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% @todo
test_sizeof() ->
    ?assertEqual(10, sizeof({a, b, c})).

test_send_msg() ->
    Channel = {erlang:make_ref(), {a, b, [{latency, 20}, {bandwith, 64}]}},
    Queues = [{Channel, []}],

    #'state'{queues=Queues1} = send_msg({a, b, c}, #'state'{queues=Queues}),

    %?assertEqual(
    %    [
    ok.


-endif.
