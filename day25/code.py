import sys
import copy
import itertools
from typing import Literal

Vertex = str
Graph = dict[Vertex, list[Vertex]]


def parse_graph(graph_txt: str) -> Graph:
    G = {}
    for line in graph_txt.splitlines():
        nam, adjs = line.split(": ")
        adjs = adjs.split()
        for adj in adjs:
            if nam not in G:
                G[nam] = []
            G[nam].append(adj)
            if adj not in G:
                G[adj] = []
            G[adj].append(nam)
    return G


# list[vertex] if path exists
# False otherwise
def find_path(
    G: Graph, s: Vertex, t: Vertex, ign: set[Vertex]
) -> Literal[False] | set[Vertex]:
    visited = set()
    parent = {}
    stack = [s]
    while stack:
        u = stack.pop(0)

        visited.add(u)

        if u == t:
            break
        for v in G[u]:
            if v not in visited and (u, v) not in ign:
                parent[v] = u
                stack.append(v)

    if t in visited:
        path = [t]
        v = t
        while v != s:
            v = parent[v]
            path.append(v)
        path.reverse()

        edges = set()
        for i in range(len(path) - 1):
            edges.add((path[i], path[i + 1]))
            edges.add((path[i + 1], path[i]))
        return edges

    return False


def find_reachable(G: Graph, s: Vertex, ign: set[Vertex]):
    visited = set()
    stack = [s]
    while stack:
        u = stack.pop(0)
        visited.add(u)
        for v in G[u]:
            if v not in visited and (u, v) not in ign:
                stack.append(v)
    return visited


def same_component(G: Graph, s: Vertex, t: Vertex, k: int):
    ign = set()

    for _ in range(k):
        p = find_path(G, s, t, ign)
        if not p:
            print("WHAT!!")
            return {s}, {t}
        ign = ign.union(p)

    if find_path(G, s, t, ign):
        print(s, t, "same component")
        return {s, t}, set()

    # different components... find all reachable vertices from s and t!

    print(s, t, "different component")
    l, r = find_reachable(G, s, ign), find_reachable(G, t, ign)
    return l, r


def find_cut(G: Graph, k1: set[Vertex], k2: set[Vertex]):
    edges = []
    for u in G.keys():
        for v in G[u]:
            if (u in k1 and v in k2) or (u in k2 and v in k1):
                if (v, u) not in edges:
                    edges.append((u, v))
    return edges


def solve(G: Graph, k: int):
    s, *V = G.keys()
    k1, k2 = {s}, set()
    i = 0

    # same_component(G, "mxr", "kdl", 3)
    for t in V:
        print(f"{i + 1}/{len(V)}")
        i += 1
        if t not in k1 and t not in k2:
            ka, kb = same_component(G, s, t, k)
            k1 = k1.union(ka)
            k2 = k2.union(kb)
    return k1, k2


def part1(file: str):
    with open(file, encoding="utf-8") as f:
        G = parse_graph(f.read())
        print(len(G.keys()))
        k1, k2 = solve(G, 3)
        print(len(k1), len(k2), len(k1) * len(k2))
        print(find_cut(G, k1, k2))


part1("example")
