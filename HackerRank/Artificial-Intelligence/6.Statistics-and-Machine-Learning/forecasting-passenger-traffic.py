#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
from scipy.interpolate import spline

def parse_input(content):
    days, data = [], []
    for line in content.split('\n'):
        day, number = line.split()
        day = int(day.split('_')[1])
        days.append(day)
        data.append(number)
    return np.array(days), np.array(data)

def visual_lines(lines):
    from matplotlib import pyplot as plt
    for day, data in lines:
        xs = np.linspace(day.min(), day.max(), 300)
        ys = spline(day, data, xs)
        plt.plot(xs, ys)
    plt.show()

if __name__ == '__main__':
    days, data = [], []
    with open('in.txt', 'r', encoding='utf-8') as fp:
        n = int(fp.readline())
        days, data = parse_input(fp.read())
    visual_lines([(days, data)])


