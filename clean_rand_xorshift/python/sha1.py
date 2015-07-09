#import math
import random
from datetime import datetime
'''
sha1 of ascii strings
'''
def strToByteArr(message): # CHANGE to use of 16 x 32-bit representation 
	res = []
	for l in message:
		res.append(ord(l) & 0xff)
	return res

def padMessage(m):
	m.append(0x80)
	while (len(m) % 64 != 56):
		m.append(0x00)
	return m

def intToArrOfEight(ml):
	ml = ml & 0xffffffffffffffff
	return [int((ml & 0xff00000000000000) >> 56), int((ml & 0xff000000000000) >> 48),
	int((ml & 0xff0000000000) >> 40), int((ml & 0xff00000000) >> 32), 
	int((ml & 0xff000000) >> 24), int((ml & 0xff0000) >> 16),
	int((ml & 0xff00) >> 8), int((ml & 0xff))]

m = ""

m = strToByteArr(m)
ml = 8 * len(m)

m = padMessage(m)

m = m + intToArrOfEight(ml)

print m

h0 = 0x67452301
h1 = 0xEFCDAB89
h2 = 0x98BADCFE
h3 = 0x10325476
h4 = 0xC3D2E1F0





print "Message Length: ", ml, "bits"