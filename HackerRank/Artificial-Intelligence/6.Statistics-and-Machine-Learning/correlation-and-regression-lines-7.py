#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

def slope(xs, ys):
    N = len(xs)
    xs, ys = np.array(xs), np.array(ys)
    a = (xs * ys).sum() - xs.sum() * ys.sum() / N
    b = (xs ** 2).sum() - xs.sum() ** 2 / N
    return a / b

if __name__ == '__main__':
    xs = [15, 12, 8, 8, 7, 7, 7, 6, 5, 3]
    ys = [10, 25, 17, 11, 13, 17, 20, 13, 9, 15]
    print(round(slope(xs, ys), 3))

