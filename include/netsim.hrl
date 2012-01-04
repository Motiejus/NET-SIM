-define(NETSIM_CLOCK, netsim_cirka).

-record('event', {
        time :: netsim_types:latency(),
        action :: add_resource | del_resource,
        nodeid :: netsim_types:nodeid(),
        resource :: netsim_types:resource()
    }).

-record(route, {
    action :: change | del,
    nodeid :: netsim_types:nodeid(), % from
    time :: netsim_types:latency(),
    route :: [netsim_types:route()],
    resource :: netsim_types:resource()
}).
