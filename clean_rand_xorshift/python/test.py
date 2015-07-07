#import math
import random
from datetime import datetime

'''
x = 4720151551 & 0xffffffff
y = 4720151551 & 0xffffffff
z = 4720151551 & 0xffffffff
w = 4720151551 & 0xffffffff
'''
x = 500000
y = 7000000
z = 13000000
w = 1700000000

def xorshift128():
	global x,y,z,w
	t = x ^ ((x << 11) & 0xffffffff)
	x = y
	y = z
	z = w
	w = w ^ ((w >> 19) & 0xffffffff) ^ t ^ ((t >> 8) & 0xffffffff)
	return w

print xorshift128()& 0xffffffff
print xorshift128()& 0xffffffff
'''
arr = [0 for i in range(10)]
for i in xrange(2**20):
	r = xorshift128() % len(arr)
	arr[r] += 1
	
print arr

arr = [0 for i in range(10)]
for i in xrange(2**20):
	r = random.randrange(0,len(arr))
	arr[r] += 1	
print arr
'''