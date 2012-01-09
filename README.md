Net Simulation Fun

Graph generator
===============

Limited for square meshs (width = height)

Produces a mesh of nodes with different parameters, suitable for netsim simulator.

Takes the following arguments:

* dimensions (number)
* latency from, to (ms)
* bandwidth from, to (bits per ms)
* nodefile (where to output node files, written below)
* channelsfile (where to output channel files, written below)

Now constants (to be implemented):

* price

Produces these text files:

* nodefile
  {nodeid(), price()}
* channelsfile
  {From :: nodeid(), To :: nodeid(), Latency :: latency(), Bandwidth :: bandwidth()}

Scenarios:

1. Mesh where all nodes connected with 4 neighbours
2. Mesh where all nodes connected with 8 neighbours

Element name format: `n<row>x<col>` starting from 0. Top-left name will always be n0x0.
