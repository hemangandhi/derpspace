import math
import random

points = [(x, 2**x) for x in range(1, 100)]

n = len(points)
x, y = zip(*points)
y = list(map(math.log, y))

m_x = sum(x) / n
m_y = sum(y) / n

s_x = math.sqrt(sum((a - m_x) ** 2 for a in x) / (n - 1))
s_y = math.sqrt(sum((a - m_y) ** 2 for a in y) / (n - 1))

z_x = map(lambda a: (a - m_x) / s_x, x)
z_y = map(lambda a: (a - m_y) / s_y, y)

r = sum(map(lambda a, b: a * b, z_x, z_y)) / (n - 1)

b = r * (s_y / s_x)

a = m_y - b * m_x

print("y = {0} * {1}^x".format(math.exp(a), math.exp(b)))
