#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

def pearson(xs, ys):
    xs, ys = np.array(xs), np.array(ys)
    a = ((xs - xs.mean()) * (ys - ys.mean())).sum()
    b = np.sqrt(((xs - xs.mean()) ** 2).sum() * ((ys - ys.mean()) ** 2).sum())
    return a / b

if __name__ == '__main__':
    xs = [15, 12, 8, 8, 7, 7, 7, 6, 5, 3]
    ys = [10, 25, 17, 11, 13, 17, 20, 13, 9, 15]
    print('%.3f' % pearson(xs, ys))

