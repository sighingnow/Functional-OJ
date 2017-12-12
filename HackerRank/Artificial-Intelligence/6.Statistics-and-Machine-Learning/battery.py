#! /usr/bin/env python3
# -*- coding: utf-8 -*-

def read_train_data(filepath):
    train_inputs, train_outputs = [], []
    with open(filepath, 'r', encoding='utf-8') as f:
        for line in f.read().splitlines():
            i, o = [float(x) for x in line.split(',')]
            train_inputs.append(i)
            train_outputs.append(o)
    return train_inputs, train_outputs

def plot(inputs, outputs):
    from matplotlib import pyplot as plt
    plt.scatter(inputs, outputs)
    plt.show()

if __name__ == '__main__':
    # inputs, outputs = read_train_data('trainingdata.txt')
    # plot(inputs, outputs)
    while True:
        try:
            x = float(input())
            if x <= 4:
                print(2 * x)
            else:
                print(8)
        except Exception:
            break

