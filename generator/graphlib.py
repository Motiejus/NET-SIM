#!/usr/bin/env python3

import unittest
import random
from math import log10, ceil

"""
This is a graph generator for netsim
Please find documentation about graph generator in README.md

Takes the following arguments:
* dimensions (number)
* latency from, to (ms)
* bandwidth from, to (bits per ms)
* nodefile (where to output node files, written below)
* channelsfile (where to output channel files, written below)

Produces these text files:
* nodefile
  {nodeid(), price()}
* channelsfile
  {From :: nodeid(), To :: nodeid(), Latency :: latency(), Bandwidth :: bandwidth()}
"""

random.seed("yadda") # this makes `random` function deterministic
ntpltpl = "{aXXXxXXX, 1}" # node template
ltpltpl = "{aXXXxXXX, aXXXxXXX, %d, %d}." # link template

def ntpl(dimensions): # node template
    return ntpltpl.replace("XXX", "%%0%(w)dd" % {'w' : ceil(log10(dimensions))})
def ltpl(dimensions):
    return ltpltpl.replace("XXX", "%%0%(w)dd" % {'w' : ceil(log10(dimensions))})


class Node:
    def __init__(self, dimensions, i, j, price):
        (self.ntpl, self.ltpl) = (ntpl(dimensions), ltpl(dimensions))
        (self.i, self.j, self.price, self.links) = (i, j, price, {})

    def chans(self):
        """Outputs channels from self to nodes that compare more than self
        
        Returns a string like this:
        ```
            {a0x0, a0x1, 20, 20}.
            {a0x0, a0x1, 20, 20}.
            {a0x0, a1x0, 20, 20}.
        '''
        """
        return "\n".join([
            self.ltpl % (self.i, self.j, n.i, n.j, l, b)
            for (n, (l, b)) in self.links.items() if self < n])
    
    def __repr__(self):
        """Outputs its name and price in this format: ```{a0x0, 1}.'''"""
        return "(%02d %02d) [%d links]" % (self.i, self.j, len(self.links))

    def ify(self):
        return self.ntpl % (self.i, self.j) + "."

    def link(self, node2, latency, bandwidth):
        """Links two nodes"""
        if node2 not in self.links.keys():
            self.links.update({node2 : (latency, bandwidth)})
        if self not in node2.links.keys():
            node2.links.update({self : (latency, bandwidth)})

    def __lt__(self, node2):
        """Nodes are compared against their fake indices. (0,0) node
        has index 0, (0,1) will have index 1, and so on. Index number is
        equivalent to C pointer position in a double dimensional array
        """
        return [self.i, self.j] < [node2.i, node2.j]


class Graph:
    def __init__(self, dimensions, latency, bandwidth, tight=False):
        self.dimensions = dimensions
        self.latency = lambda: random.randint(latency[0], latency[1])
        self.bandwidth = lambda: random.randint(bandwidth[0], bandwidth[1])
        self.price = lambda: 1
        self.nodes = [None for x in range(dimensions)]
        self.tight = tight
        self.generate()

    def generate(self):
        for i in range(self.dimensions):
            self.nodes[i] = [None for x in range(self.dimensions)]
            for j in range(self.dimensions):
                self.nodes[i][j] = Node(self.dimensions, i, j, self.price())

        for i in range(self.dimensions):
            for j in range(self.dimensions):
                d = self.dimensions
                todo = []
                if i != 0:     todo.append((-1,  0,))
                if i != d - 1: todo.append(( 1,  0,))
                if j != 0:     todo.append(( 0, -1,))
                if j != d - 1: todo.append(( 0,  1,))

                if self.tight:
                    if i != 0 and j != 0:         todo.append((-1, -1,))
                    if i != 0 and j != d - 1:     todo.append((-1,  1,))
                    if i != d - 1 and j != 0:     todo.append(( 1, -1,))
                    if i != d - 1 and j != d - 1: todo.append(( 1,  1,))

                for (x, y) in todo:
                    self.nodes[i][j].link(self.nodes[i+x][j+y],
                            self.latency(), self.bandwidth())

    def nodefile(self):
        """Outputs node list in this format (one multi-line string):
            {a0x0, 10}.
            {a0x1, 10}.
        """
        return "\n".join([n.ify() for nl in self.nodes for n in nl])

    def chansfile(self):
        """Outputs channels in this format:
            {a0x0, a0x1, 20, 20}.
            {a0x0, a0x1, 20, 20}.
            {a0x0, a1x0, 20, 20}.
        """
        return "\n".join([n.chans() for nl in self.nodes for n in nl])

    def simulation_file(self):
        Node = ntpl(self.dimensions) % (0, 0)
        return "{'event', 1, add, %s}.\n"\
                "{'event', 2, del, %s}." % (Node, Node)

    def settings_file(self):
        return "{max_latency, 20000}.\n" \
                "{monitor_resource, %s}." % ntpl(self.dimensions) % (0, 0)

    # with Graph(...) as g:
    def __enter__(self):
        return self
    def __exit__(self, type, value, traceback):
        pass


class TestNodeCase(unittest.TestCase):
    def test_2x2_mesh(self):
        graph = Graph(dimensions=2, latency=(5,5), bandwidth=(10,10))
        nodes = graph.nodes
        [self.assertEqual(len(n.links), 2) for r in nodes for n in r]
        self.assertTrue(nodes[0][1] in nodes[0][0].links.keys())
        self.assertTrue(nodes[1][0] in nodes[0][0].links.keys())
        self.assertFalse(nodes[1][1] in nodes[0][0].links.keys())
        f = "\n".join([ntpl(2) % (i, j) for i in [0,1] for j in [0,1]])
        self.assertEqual(f, graph.nodefile())
        self.assertIn(ltpl(2) % (0,0, 0,1, 5,10), graph.chansfile())
        self.assertIn(ltpl(2) % (0,1, 1,1, 5,10), graph.chansfile())

    def test_3x3_mesh(self):
        graph = Graph(dimensions=3, latency=(5,5), bandwidth=(10,10))
        nodes = graph.nodes

        corners = [nodes[i][j] for i in [0,-1] for j in [0,-1]]
        threes = [nodes[0][1], nodes[1][0], nodes[1][2], nodes[2][1]]
        center = nodes[1][1]
        [self.assertEqual(len(r.links.keys()), 2) for r in corners]
        [self.assertEqual(len(r.links.keys()), 3) for r in threes]
        self.assertEqual(len(center.links), 4)
        self.assertTrue(nodes[0][1] in nodes[0][0].links.keys())
        self.assertTrue(nodes[1][0] in nodes[0][0].links.keys())
        self.assertFalse(nodes[1][1] in nodes[0][0].links.keys())
        self.assertFalse(nodes[1][2] in nodes[0][0].links.keys())

        self.assertTrue(nodes[0][0] in nodes[0][1].links.keys())
        self.assertTrue(nodes[1][1] in nodes[0][1].links.keys())

if __name__ == "__main__":
    unittest.main()
