-module(netsim_types).

-type latency() :: pos_integer().
-type nodeid() :: pos_integer().
-type price() :: pos_integer().
-type metric_attribute() :: {latency, latency()} | {price, integer()} |
                            {bandwidth, integer()}.
-type metrics() :: [metric_attribute()].
-type channel() :: {Id :: reference(),  {From :: nodeid(), To :: nodeid(),
                    Metrics :: metrics()}}.
-type resource() :: pos_integer().
-type path() :: [channel()].
-type cost() :: {latency(), price()}.
-type route() :: {resource(), path(), cost()}.
-type route_table() :: [route()].
-type msg_queue() :: [{channel(), [{TimeLeft :: pos_integer(), route()}] }].

-export_types([latency/0, nodeid/0, metric_attribute/0, metrics/0,
        channel/0, resource/0, path/0, cost/0, route/0, route_table/0,
        price/0, msg_queue/0]).
