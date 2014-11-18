#!/usr/bin/env python2

import os
import sys
import argparse

import subprocess
import re
import random

class Edge ():
    def __init__(self, s, e):
        self.start = s
        self.end = e
        if s == e:
            self.capacity = 0
        else:
            self.capacity = random.randint(1, 10)
        self.usage = 0
    def write(self):
        toPrint = "start: " + str(self.start) + " end: " + str(self.end) + " "
        toPrint = toPrint + "c: " + str(self.capacity) + " "
        toPrint = toPrint + "u: " + str(self.usage)
        print toPrint
    def fileout(self, fh):
        if self.capacity != 0:
            fh.write(str(self.start) + " " + str(self.end) + " " + str(self.capacity) + "\n")

class Graph ():
    def read_from_file(self, path):
        fh = open(path, 'r')
        self.nodes = int(fh.readline())
        self.edges = int(fh.readline())

        self.graph = [[Edge(i, j) for j in range(self.nodes)] for i in range(self.nodes)]
        #zero the capacities
        for row in self.graph:
            for e in row:
                e.capacity = 0
        alledges = fh.readlines()
        for line in alledges:
            values = line.split(' ')
            s = int(values[0])
            e = int(values[1])
            c = int(values[2])
            self.graph[s][e].capacity = c
        fh.close()
    def create_random(self, n):
        self.nodes = n
        self.graph = [[Edge(i, j) for j in range(n)] for i in range(n)]
        edgect = 0
        for row in self.graph:
            for e in row:
                if e.capacity > 0:
                    edgect = edgect +1 
        self.edges = edgect
    def __init__(self, n = None, path = None):
        if n is None and path is not None:
            self.read_from_file(path)
        elif n is not None and path is None:
            self.create_random(n)
        else:
            raise Exception("error")
    def write(self):
        for row in self.graph:
            for e in row:
                e.write()
    def fileout(self, destination):
        fht = open(destination, 'w')
        fht.write(str(self.nodes)+"\n")
        fht.write(str(self.edges)+"\n")
        for row in self.graph:
            for e in row:
                e.fileout(fht)
        fht.close()
    def pe(self, s, e):
        self.graph[s][e].write()

#argument parsing
a = argparse.ArgumentParser(description='validate flows for max flow assignment')
a.add_argument("--binary", help='binary to use', default = './flow', dest="bin")
a.add_argument("--ref", help='reference solution to use', default = './flow_reference', dest="ref")
a.add_argument("--size", help='number of nodes in the graph', default = 4, dest="size")
a.add_argument("--seed", help='random seed', default = 4, dest="seed")
a.add_argument("--graph", help='path for special graph to use.  If this is set, that graph is used and size is ignored', default = None, dest="graph")

arguments = a.parse_args()
random.seed(arguments.seed)

if arguments.graph is None:
    g = Graph(n = int(arguments.size))
else:
    g = Graph(path = arguments.graph)


graph_path = 'graph2.flow'

g.fileout(graph_path)

#running reference solution
command = [arguments.ref]
f = open(graph_path, "r")
reference_p = subprocess.Popen(command, stdin=f, stdout=subprocess.PIPE)
reference_p.wait()
f.close();
reference_output = reference_p.communicate()[0]
reference_output = reference_output.split('\n')
reference_flow = int(reference_output[-2])


#running student solution
command = [arguments.bin]
f = open(graph_path, "r")
student_p = subprocess.Popen(command, stdin=f, stdout=subprocess.PIPE)
student_p.wait()
f.close();
student_output = student_p.communicate()[0]
student_output = student_output.split('\n')
student_flow = int(student_output[-2])

if student_flow != reference_flow:
    print "flow sizes do not match"
    print "reference flow is " + str(reference_flow) + " and student flow is " + str(student_flow)
else:
    print "flow magnitude matches with a value of " + str(reference_flow)

for i in range(len(student_output) - 2):
    values = student_output[i].split(' ')
    s = int(values[0])
    e = int(values[1])
    u = int(values[2])
    if g.graph[s][e].usage != 0:
        print "error - this edge is listed in the output more than once"
        g.graph[s][e].write()
    elif g.graph[s][e].capacity < u:
        print "Error: capacity constraint violated on this edge:"
        g.graph[s][e].write()
        print "tried to give it a capacity of " + str(u)
    else:
        g.graph[s][e].usage = u

print "done validating capacity constraints"

#vertex 0 is the source, so the outgoing edges should sum to the flow.
#incoming edges should all have usage of 0
outgoing = 0
for i in range(g.nodes):
    outgoing = outgoing + g.graph[0][i].usage
    if g.graph[i][0].usage != 0:
        print "the source vertex has incoming flow"
if outgoing != reference_flow:
    print "outgoing supply from the source doesn't match flow"

#vertex 1 is the sink, so the incoming edges should sum to the flow.
#outgoing edges should all have usage of 0
incoming = 0
for i in range(g.nodes):
    incoming = incoming + g.graph[i][1].usage
    if g.graph[1][i].usage != 0:
        print "the sink vertex has outgoing flow"
if incoming != reference_flow:
    print "incoming supply from the sink doesn't match flow"

for i in range(2, g.nodes):
    incoming = 0
    outgoing = 0
    for j in range(g.nodes):
        incoming = incoming + g.graph[j][i].usage
        outgoing = outgoing + g.graph[i][j].usage
    if incoming != outgoing:
        print "the outgoing flow from vertex " + str(i) + "does not match the incoming flow"
        print "outgoing: " + str(outgoing)
        print "incoming: " + str(incoming)
