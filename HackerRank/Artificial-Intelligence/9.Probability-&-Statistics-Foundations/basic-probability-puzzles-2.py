#! /usr/bin/env python3
# -*- coding: utf-8 -*-

def calculate():
    s = 0
    for i in range(1, 7):
        for j in range(1, 7):
            if i != j and i + j == 9:
                s += 1
    return s, 36

if __name__ == '__main__':
    a, b = calculate()
    print('%d/%d' % (a, b))

