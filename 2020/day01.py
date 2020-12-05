#!/usr/bin/python3

import sys

with open(sys.argv[1]) as input:
    lines = input.readlines()

data = list(map(lambda x: int(x), lines))

for i in range(len(data)):
    for element in data[i+1:]:
        if(data[i] + element == 2020):
            print("i={} data[i]={} element={} i+element={} i*element={}".format(i, data[i], element, data[i]+element, data[i]*element))


for i in range(len(data)-2):
    for j in range(i+1, len(data)-1):
        for k in range(j+1, len(data)):
            if(data[i] + data[j] + data[k] == 2020):
                print("i={} j={} k={} i+j+k={} i*j*k={}".format(i, j, k, data[i]+data[j]+data[k], data[i]*data[j]*data[k]))
