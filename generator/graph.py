#!/usr/bin/env python

"""
API for creating and dumping the graph using graphlib
"""
from os import path, makedirs
from graphlib import Graph

def run():
    # 10x10 referece , all fixed
    with Graph(dimensions=10, latency=(5, 5), bandwidth=(128, 128)) as g:
        write_graph_data(g, "reference_model")

    # 10x10 higher latency, all fixed
    with Graph(dimensions=10, latency=(10, 10), bandwidth=(128, 128)) as g:
        write_graph_data(g, "higher_latency")

    # 10x10 higher speed, all fixed
    with Graph(dimensions=10, latency=(5, 5), bandwidth=(512, 512)) as g:
        write_graph_data(g, "higher_speed")

    # 10x10 more links , all fixed
    with Graph(dimensions=10, latency=(5, 5), bandwidth=(128, 128),tight=True) as g:
        write_graph_data(g, "tight_links")

    # 10x10 long distance links, all fixed
    with Graph(dimensions=10, latency=(2, 5), bandwidth=(128, 128)) as g:
        g.nodes[0][0].link(g.nodes[2][5], latency=8, bandwidth=128)
        g.nodes[2][0].link(g.nodes[5][7], latency=8, bandwidth=128)
        write_graph_data(g, "long_distance")

    # 50x50 tight order, all fixed
    with Graph(dimensions=50, latency=(2, 5), bandwidth=(50, 500),
            tight=True) as g:
        write_graph_data(g, "huge_net")


PROJECT_DIR = path.abspath(path.dirname(path.dirname(__file__)))
RES_DIR = path.join(PROJECT_DIR, 'res')

### Helpers
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

if __name__ == "__main__":
    run()
