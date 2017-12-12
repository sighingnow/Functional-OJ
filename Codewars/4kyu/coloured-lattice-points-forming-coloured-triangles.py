#!/usr/bin/env python
# -*- coding: utf-8 -*-

def triang(a, b, c):
    det = a[0] * (b[1] - c[1]) + b[0] * (c[1] - a[1]) + c[0] * (a[1] - b[1])
    return det != 0

def count_triang(edges):
    nlen = len(edges)
    if nlen <= 2:
        return 0
    count = 0
    for i in range(0, nlen - 2):
        for j in range(i + 1, nlen - 1):
            for k in range(j + 1, nlen):
                if triang(edges[i], edges[j], edges[k]):
                    count += 1
    return count

def count_col_triang(input_):
    edges = {}
    for e in input_:
        if edges.get(e[1]):
            edges[e[1]].append(e[0])
        else:
            edges[e[1]] = [e[0]]
    k, r, color = 0, 0, []
    for c, es in edges.items():
        ts = edges[c] = count_triang(es)
        k += ts
        if ts > r:
            r = ts
    for c, es in edges.items():
        if es == r:
            color.append(c)
    color.sort()
    if r == 0:
        return [len(input_), len(edges), k, []]
    else:
        color.append(r)
        return [len(input_), len(edges), k, color]
