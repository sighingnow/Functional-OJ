#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import math

def calculate():
    a = 3 * 5 * 4 + 4 * 4 * 4 + 4 * 5 * 4
    b = 7 * 9 * 8
    x = math.gcd(a, b)
    return a / x, b / x

if __name__ == '__main__':
    a, b = calculate()
    print('%d/%d' % (a, b))

