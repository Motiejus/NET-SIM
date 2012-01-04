%%% @doc netsim_serv_tests
-module(netsim_tests).
-include("include/log_utils.hrl").
-include("include/netsim.hrl").
-include("include/netsim_serv.hrl").

-include_lib("eunit/include/eunit.hrl").

send_route_sync_test_() ->
    {foreach,
        fun meck_setup/0,
        fun meck_cleanup/1,
        [
            {"Send new route synchronously", fun meck_send_route_sync/0}
        ]
    }.

meck_setup() ->
    meck:new(netsim_serv, [passthrough]),
    meck:expect(netsim_serv, handle_cast, fun
            ({route, #route{action=change}, ReportCompleteTo},
                State=#state{nodeid=NodeId}) ->
                gen_server:cast(ReportCompleteTo, {update_complete, NodeId}),
                {noreply, State};
            (What, State) ->
                meck:passthrough([What, State])
        end),
    application:start(sasl),
    application:start(netsim).

meck_cleanup(_) ->
    meck:unload(netsim_serv),
    ?mute_log(),
    application:stop(netsim),
    application:stop(sasl),
    ?unmute_log().

meck_send_route_sync() ->
    ok.
