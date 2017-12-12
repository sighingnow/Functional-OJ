#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import math

def mean(N, xs):
    return sum(xs) / N

def minus(xs, k):
    return [x - k for x in xs]

def pow(xs, k):
    return [x ** k for x in xs]

def multiply(xs, ys):
    return [a * b for a, b in zip(xs, ys)]

def pearson(n, xs, ys):
    mxs = mean(n, xs)
    mys = mean(n, ys)
    a = sum(multiply(minus(xs, mxs), minus(ys, mys)))
    b = math.sqrt(sum(pow(minus(xs, mxs), 2)) * sum(pow(minus(ys, mys), 2)))
    return a / b

if __name__ == '__main__':
    N = int(input())
    xs, ys, zs = [], [], []
    for _ in range(N):
        m, p, c = [int(x) for x in input().split()]
        xs.append(m)
        ys.append(p)
        zs.append(c)
    print('%.2f' % pearson(N, xs, ys))
    print('%.2f' % pearson(N, ys, zs))
    print('%.2f' % pearson(N, xs, zs))

