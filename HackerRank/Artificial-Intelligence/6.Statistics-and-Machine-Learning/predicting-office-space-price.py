#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from sklearn.preprocessing import PolynomialFeatures
from sklearn import linear_model

def fit(degree, inputs, outputs):
    poly = PolynomialFeatures(degree = degree)
    poly_inputs = poly.fit_transform(inputs)
    model = linear_model.LinearRegression()
    model.fit(poly_inputs, outputs)
    return (poly, model)

def predict(model, poly, inputs):
    return model.predict(poly.fit_transform(inputs))

if __name__ == '__main__':
    F, N = [int(x) for x in input().split()]
    train_data = [[float(x) for x in input().split()] for _ in range(N)]
    inputs = [row[0:F] for row in train_data]
    outputs = [row[F] for row in train_data]
    T = int(input())
    test_inputs = [[float(x) for x in input().split()] for _ in range(T)]

    poly, model = fit(3, inputs, outputs) ## the degree 3 is important
    result = predict(model, poly, test_inputs)

    for r in result:
        print(r)

