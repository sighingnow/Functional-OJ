#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# ref: https://en.wikipedia.org/wiki/Stochastic_oscillator

import numpy as np

def stochastic_oscillator(ns):
    assert(len(ns) == 5)
    L5, H5 = ns.min(), ns.max()
    K = (ns - L5) / (H5 - L5)
    K1, K2, K3 = ns[0:3].sum(), ns[1:4].sum(), ns[2:5].sum()
    return K1 * 0.25 + K2 * 0.5 + K3 * 0.25

def printTransactions(m, k, d, name, owned, prices):
    prices = np.array(prices)
    for p in prices:
        print(p)
        # print(p.std(1))
        print(stochastic_oscillator(p))

if __name__ == '__main__':
    m, k, d = [float(i) for i in input().strip().split()]
    k = int(k)
    d = int(d)
    names = []
    owned = []
    prices = []
    for data in range(k):
        temp = input().strip().split()
        names.append(temp[0])
        owned.append(int(temp[1]))
        prices.append([float(i) for i in temp[2:7]])

    printTransactions(m, k, d, names, owned, prices)

