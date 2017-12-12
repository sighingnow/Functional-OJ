#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import math

def calculate():
    a = 4 * (7 * 6 // 2) + 5 * (3 * 7)
    b = 9 * (10 * 9 // 2)
    x = math.gcd(a, b)
    return a / x, b / x

if __name__ == '__main__':
    a, b = calculate()
    print('%d/%d' % (a, b))

