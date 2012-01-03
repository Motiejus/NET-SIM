-module(netsim_types).

-type latency() :: pos_integer().
-type nodeid() :: atom().
-type price() :: integer().
-type bandwidth() :: pos_integer().
-type resource() :: {nodeid(), pos_integer()}.
-type metric_attribute() :: {latency, latency()} | {bandwidth, bandwidth()}.
-type metrics() :: [metric_attribute()].
-type link() :: {From :: nodeid(), To :: nodeid(), Metrics :: metrics()}.
-type path() :: [nodeid()].
-type cost() :: {latency(), price()}.
-type route() :: {resource(), path(), cost()}.
-type route_table() :: [{CurrentRoute :: route(), History :: [route()]}].
-type msg_queue() :: {link(), [{TimeLeft :: pos_integer(), Msg :: term()}]}.

-export_types([latency/0, nodeid/0, metric_attribute/0, metrics/0,
        resource/0, path/0, cost/0, route/0, route_table/0,
        price/0, msg_queue/0]).
