-define(NETSIM_CLOCK, netsim_cirka).

-record('event', {
        time :: netsim_types:latency(),
        action :: netsim_types:action(),
        resource :: netsim_types:resource()
    }).

-record(route, {
    action :: change | del,
    nodeid :: netsim_types:nodeid(), % from
    time :: netsim_types:latency(),
    route :: [netsim_types:route()],
    resource :: netsim_types:resource()
}).

-record(stat, {
        tick :: netsim_types:latency(),
        action :: change | del | traffic | stop | total_traffic,
        resource :: netsim_types:resource(), % for which resource statistics
        nodeid :: netsim_types:nodeid(), % who sent this data
        tx :: netsim_types:bits(),
        rx :: netsim_types:bits()
    }).

-define(NETSIM_PUBSUB, netsim_pubsub). % pg2 group name

-record(log, {
    events = [] :: list(),
    traffic = [] :: list(),
    ticks = [] :: list(),
    total_traffic = [] :: list()
}).
