-module(netsim_serv).

-behaviour(gen_server).

-export([start_link/2, add_channel/2]).

-export([init/1, handle_cast/2, handle_call/3, code_change/3,
        handle_info/2, terminate/2]).

-record(state, {
        queue :: netsim_types:msg_queue(),
        nodeid :: netsim_types:nodeid(),
        table :: netsim_types:route_table(),
        cost :: netsim_types:cost(),
        channels = [] :: netsim_types:channels()
    }).

%% =============================================================================

start_link(Nodeid, Cost) ->
    gen_server:start_link({local, Nodeid}, ?MODULE, [Nodeid, Cost], []).

-spec add_channel(netsim_types:nodeid(), netsim_types:channel()) -> ok.
add_channel(NodeId, Channel) ->
    gen_server:call(NodeId, {add_channel, Channel}).

%% =============================================================================

init([Nodeid, Cost]) ->
    io:format("~p~n", [Nodeid]),
    {ok, #state{nodeid=Nodeid, cost=Cost, queue=[], table=[]}}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_call({add_channel, {Id, Info}=Channel}, _From,
        #'state'{channels=Channels}=State) ->

    Channels1 = 
        case proplists:get_value(Id, Channels, '$undefined') of 
            '$undefined' ->
                % @todo
                do_smthng;
            Info ->
                ok; % channel already exists
            NewRouteInfo ->
                throw(not_unique_channel_id)
        end,

    {ok, State#'state'{channels=Channels}};

handle_call(Msg, _From, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    {noreply, State}.

terminate(normal, State) ->
    State.

code_change(_, _, State) ->
    {ok, State}.
