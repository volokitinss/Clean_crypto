implementation module sha1


import	StdInt, StdClass
import StdTuple
import StdList
import StdString
import StdArray

bytesToInts :: [Int] -> [Int]//group ints 0-255 to 2**32 ints and pad the last with bits "10..0"
bytesToInts []		= [0x80000000]
bytesToInts [i]		= [(i bitand 0xff) << 24 bitor 0x800000]
bytesToInts [i,i1]		= [((i bitand 0xff) << 24) bitor ((i1 bitand 0xff) << 16) bitor 0x8000]
bytesToInts [i,i1,i2]		= [((i bitand 0xff) << 24) bitor ((i1 bitand 0xff) << 16) bitor ((i2 bitand 0xff) << 8) bitor 0x80]
bytesToInts [i,i1,i2,i3]		= [((i bitand 0xff) << 24) bitor ((i1 bitand 0xff) << 16) bitor ((i2 bitand 0xff) << 8) bitor (i3 bitand 0xff),  0x80000000]
bytesToInts [i:i1:i2:i3:is]		= [((i bitand 0xff) << 24) bitor ((i1 bitand 0xff) << 16) bitor ((i2 bitand 0xff) << 8) bitor (i3 bitand 0xff): bytesToInts is]

strToIntArr :: String -> [Int]
strToIntArr ""		= []
strToIntArr s	= [toInt s.[0]: strToIntArr (s % (1, size s)) ]

sizeOfMess :: String -> [Int]
sizeOfMess s	= [(si >> 32) bitand 0xffffffff,  (si bitand 0xffffffff)]
where
	si = (size s) * 8 

padWithZerosTo448 :: [Int] -> [Int]
padWithZerosTo448 a
| (length a) rem 16 == 14	= a								// x rem 16 == 14 checks if length of list in bites is k*512+448
| otherwise					= padWithZerosTo448 (a ++ [0])

chunck512WithSize :: String -> [Int] // return array with length which is multiple of 512 and it has last 64 bits of size of original string
chunck512WithSize s = (padWithZerosTo448 (bytesToInts (strToIntArr s))) ++ (sizeOfMess s)


Start = chunck512WithSize ""