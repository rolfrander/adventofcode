#!/usr/bin/python3

import numpy as np
import re

re_turn_on  = re.compile("^turn on ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)")
re_turn_off = re.compile("^turn off ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)")
re_toggle   = re.compile("^toggle ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)")


class lightarray(object):
    def __init__(self, dim):
        self.lights = np.zeros([dim,dim], dtype=np.short)
        self.MAX = 255*127

    def subarray(self, x1, y1, x2, y2):
        return self.lights[x1:(x2+1),y1:(y2+1)]

    def subarray_from_match(self, match):
        x1=int(match.group(1))
        y1=int(match.group(2))
        x2=int(match.group(3))
        y2=int(match.group(4))
        return self.subarray(x1,y1,x2,y2)

    def count(self):
        return np.sum(self.lights)

    def turn_on(self, array):
        array.fill(-1)

    def turn_off(self, array):
        array.fill(0)

    def toggle(self, array):
        np.invert(array, array)

    def exec_line(self, line):
        m = re_turn_on.match(line)
        if m:
            self.turn_on(self.subarray_from_match(m))
            return
        m = re_turn_off.match(line)
        if m:
            self.turn_off(self.subarray_from_match(m))
            return
        m = re_toggle.match(line)
        if m:
            self.toggle(self.subarray_from_match(m))
            return

class lightarray2(lightarray):
    def __init__(self, dim):
        super(lightarray2, self).__init__(dim)

    def turn_on(self, array):
        np.add(array, 1, array)

    def turn_off(self, array):
        np.add(array, -1, array)
        np.clip(array, 0, self.MAX, array)

    def toggle(self, array):
        np.add(array, 2, array)


l=lightarray(1000)
l.exec_line("turn on 0,0 through 999,999")
l.exec_line("toggle 0,0 through 999,0")
l.exec_line("turn off 499,499 through 500,500")
print(l.count())

l=lightarray2(1000)
l.exec_line("turn on 0,0 through 0,0")
l.exec_line("toggle 0,0 through 999,999")
print(l.count())

lights = lightarray2(1000)
with open('input06.txt') as f:
    for line in f.readlines():
        lights.exec_line(line)

print(lights.count())




