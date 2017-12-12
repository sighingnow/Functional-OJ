#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

def mean(xs):
    return xs.mean()

def median(xs):
    xs = np.sort(xs)
    if len(xs) % 2 == 1:
        return xs[len(xs) // 2]
    else:
        return (xs[len(xs) // 2 - 1 : len(xs) // 2 + 1].mean())

def mode(xs):
    return np.bincount(xs).argmax()

def sd(xs):
    return xs.std()

def lowerupper(n, xs):
    m = xs.mean()
    std = xs.std()
    nlen = n ** 0.5
    return m - 1.96 * std / nlen, m + 1.96 * std / nlen

if __name__ == '__main__':
    n = int(input())
    xs = np.array([int(x) for x in input().split()])
    print(round(mean(xs), 1))
    print(round(median(xs), 1))
    print(mode(xs))
    print(round(sd(xs), 1))
    l, u = lowerupper(n, xs)
    print('%.1f %.1f' % (round(l, 1), round(u, 1)))

