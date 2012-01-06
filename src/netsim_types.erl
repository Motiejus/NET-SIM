-module(netsim_types).

-include("include/netsim.hrl").

-type action() :: add | del.
-type latency() :: pos_integer().
-type nodeid() :: atom().
-type price() :: integer().
-type bandwidth() :: pos_integer().
-type bits() :: non_neg_integer().
-type resource() :: {Node :: nodeid(), ResNum :: pos_integer()}.
-type metric_attribute() :: {latency, latency()} | {bandwidth, bandwidth()}.
-type metrics() :: [metric_attribute()].
-type link() :: {From :: nodeid(), To :: nodeid(), Metrics :: metrics(),
                {TX :: bits(), RX :: bits()}}.
-type path() :: [nodeid()].
-type cost() :: {latency(), price()}.
-type route() :: {path(), cost()}.
-type route_table() :: [{resource(), [route()]}]. % Head of [route()] is the
%% most optimal route.
-type msg_queue() :: {link(), [{Msg :: #route{}, TimeLeft :: pos_integer()}]}.
-type results() :: [{add|del, [{netsim_types:latency(), pos_integer()}]}].

-export_types([latency/0, nodeid/0, metric_attribute/0, metrics/0,
        resource/0, path/0, cost/0, route/0, route_table/0,
        price/0, msg_queue/0, bits/0]).
