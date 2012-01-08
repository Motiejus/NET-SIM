#!/usr/bin/env python

"""
API for creating and dumping the graph using graphlib
"""
from os import path, makedirs
from graphlib import Graph

PROJECT_DIR = path.abspath(path.dirname(path.dirname(__file__)))
RES_DIR = path.join(PROJECT_DIR, 'res')

def write_graph_data(graph):
    DIR = path.join(RES_DIR, '%dx%d' % (graph.dimensions, graph.dimensions))
    makedirs(DIR)
    with open(path.join(DIR, 'nodefile.txt'), 'w') as f:
        f.write(graph.nodefile())
        f.close()
    with open(path.join(DIR, 'channels.txt'), 'w') as f:
        f.write(graph.chansfile())
        f.close()
    with open(path.join(DIR, 'simulation.txt'), 'w') as f:
        f.write(graph.simulation_file())
    with open(path.join(DIR, 'settings.txt'), 'w') as f:
        f.write(graph.settings_file())

if __name__ == "__main__":
    # Create and dump simple 5x5 graph
    graph_5x5 = Graph(dimensions=5, latency=(5, 5), bandwidth=(100, 100))
    write_graph_data(graph_5x5)
