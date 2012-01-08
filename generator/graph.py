#!/usr/bin/env python

"""
API for creating and dumping the graph using graphlib
"""
from os import path, makedirs
from graphlib import Graph

PROJECT_DIR = path.abspath(path.dirname(path.dirname(__file__)))
RES_DIR = path.join(PROJECT_DIR, 'res')

def write_graph_data(graph, name):
    DIR = path.join(RES_DIR, name)
    try:
        makedirs(DIR)
    except OSError: # directory probably exists, overwriting
        pass
    with open(path.join(DIR, 'nodelist.txt'), 'w') as f:
        f.write(graph.nodefile())
    with open(path.join(DIR, 'channels.txt'), 'w') as f:
        f.write(graph.chansfile())
    with open(path.join(DIR, 'simulation.txt'), 'w') as f:
        f.write(graph.simulation_file())
    with open(path.join(DIR, 'settings.txt'), 'w') as f:
        f.write(graph.settings_file())

class mkgraph:
    def __init__(self, **kargs):
        self.kargs = kargs
    def __enter__(self):
        return Graph(**self.kargs)
    def __exit__(self, type, value, traceback):
        pass

if __name__ == "__main__":
    # 8x8 nice order, all fixed
    with mkgraph(dimensions=8, latency=(5, 5), bandwidth=(100, 100)) as g:
        write_graph_data(g, "8x8_a_fix")

    # 8x8 modified order, all fixed
    with mkgraph(dimensions=8, latency=(5, 5), bandwidth=(100, 100)) as g:
        g.nodes[0][0].link(g.nodes[2][5], latency=8, bandwidth=500)
        g.nodes[5][0].link(g.nodes[5][7], latency=8, bandwidth=500)
        write_graph_data(g, "8x8_b_fix")

    # 8x8 tight order, all fixed
    with mkgraph(dimensions=8, latency=(5, 5), bandwidth=(100, 100),
            tight=True) as g:
        write_graph_data(g, "8x8_t_fix")

    # 8x8 nice order, some randomness
    with mkgraph(dimensions=8, latency=(2, 5), bandwidth=(50, 500)) as g:
        write_graph_data(g, "8x8_a_rand")

    # 8x8 modified order, some randomness
    with mkgraph(dimensions=8, latency=(2, 5), bandwidth=(50, 500)) as g:
        g.nodes[0][0].link(g.nodes[2][5], latency=8, bandwidth=500)
        g.nodes[5][0].link(g.nodes[5][7], latency=8, bandwidth=500)
        write_graph_data(g, "8x8_m_rand")

    # 8x8 tight order, some randomness
    with mkgraph(dimensions=8, latency=(2, 5), bandwidth=(50, 500),
            tight=True) as g:
        write_graph_data(g, "8x8_t_rand")


    ## 100x100 nice order, all fixed
    #with mkgraph(dimensions=100, latency=(5, 5), bandwidth=(100, 100)) as g:
    #    write_graph_data(g, "100x100_a_fix")

    ## 100x100 modified order, all fixed
    #with mkgraph(dimensions=100, latency=(5, 5), bandwidth=(100, 100)) as g:
    #    g.nodes[0][0].link(g.nodes[20][30], latency=8, bandwidth=500)
    #    g.nodes[10][0].link(g.nodes[40][0], latency=8, bandwidth=500)
    #    write_graph_data(g, "100x100_b_fix")

    ## 100x100 tight order, all fixed
    #with mkgraph(dimensions=100, latency=(5, 5), bandwidth=(100, 100),
    #        tight=True) as g:
    #    write_graph_data(g, "100x100_a_fix")

    ## 100x100 nice order, some randomness
    #with mkgraph(dimensions=100, latency=(2, 5), bandwidth=(50, 500)) as g:
    #    write_graph_data(g, "100x100_a_rand")

    ## 100x100 modified order, some randomness
    #with mkgraph(dimensions=100, latency=(2, 5), bandwidth=(50, 500)) as g:
    #    g.nodes[0][0].link(g.nodes[20][30], latency=8, bandwidth=500)
    #    g.nodes[10][0].link(g.nodes[40][0], latency=8, bandwidth=500)
    #    write_graph_data(g, "100x100_m_rand")

    ## 100x100 tight order, some randomness
    #with mkgraph(dimensions=100, latency=(2, 5), bandwidth=(50, 500),
    #        tight=True) as g:
    #    write_graph_data(g, "100x100_t_rand")
