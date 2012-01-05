-record(state, {
        queues = [] :: [netsim_types:msg_queue()],
        nodeid :: netsim_types:nodeid(),
        table :: netsim_types:route_table(),
        price :: netsim_types:price(),
        tick = -1 :: non_neg_integer(), % current tick
        max_latency :: pos_integer(), % max acceptable latency
        pending_responses = [] :: [netsim_types:nodeid()]
    }).
