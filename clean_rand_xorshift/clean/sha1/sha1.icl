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

// transformes string to array of ints in range 0-255
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

// return array with length which is multiple of 512 and it has last 64 bits of size of original string
chunck512WithSize :: String -> [Int]
chunck512WithSize s = (padWithZerosTo448 (bytesToInts (strToIntArr s))) ++ (sizeOfMess s)

//function takes an array of intermidiate results hi and padded message. hi initially = [0x67452301,0xEFCDAB89, 0x98BADCFE, 0x10325476,0xC3D2E1F0]
process512Chunk :: [Int] [Int] -> [Int]
process512Chunk hi []	= hi
process512Chunk hi i	= process512Chunk abcde (drop 16 i)
where
	abcde = (chunkHash 0 hi hi (extend512to80x32 (take 16 i)))


extend512to80x32 :: [Int] -> [Int]
extend512to80x32 x = extIter 16 x

// rotates 32-bit integer left by shift
leftrotate32 :: Int Int -> Int
leftrotate32 shift x = ((x << shift) bitand 0xffffffff) bitor h
where
	h = ((x bitand 0xffffffff) >> (32-shift))
	
// Thr first param is a loop iteration, the second is intermidiate result, and the last is chunk of data 80x32 bit
chunkHash :: Int [Int] [Int] [Int] -> [Int]
chunkHash 80 hi abcde _ = vSum hi abcde
chunkHash n hi abcde ch
| n < 20 	= chunkHash (n+1) hi (elRot abcde f1 k1 (ch!!n)) ch
| n < 40 	= chunkHash (n+1) hi (elRot abcde f2 k2 (ch!!n)) ch
| n < 60 	= chunkHash (n+1) hi (elRot abcde f3 k3 (ch!!n)) ch
| otherwise	= chunkHash (n+1) hi (elRot abcde f4 k4 (ch!!n)) ch
where
	f1 = (abcde!!1) bitand (abcde!!2) bitor ((bitnot (abcde!!1))bitand (abcde!!3))//(b and c) or ((not b) and d)
	f2 = (abcde!!1) bitxor (abcde!!2) bitxor (abcde!!3) // b xor c xor d
	f3 = (abcde!!1 bitand abcde!!2) bitor (abcde!!1 bitand abcde!!3) bitor (abcde!!2 bitand abcde!!3) // (b and c) or (b and d) or (c and d)
	f4 = abcde!!1 bitxor abcde!!2 bitxor abcde!!3// b xor c xor d
	k1 = 0x5A827999
	k2 = 0x6ED9EBA1
	k3 = 0x8F1BBCDC
	k4 = 0xCA62C1D6

// Process elements and returns array of 5 ints
elRot :: [Int] Int Int Int -> [Int]
elRot abcde f k chn = [(((leftrotate32 5 (abcde!!0)) + f + k + (abcde!!4) + chn) bitand 0xffffffff),(abcde!!0),(leftrotate32 30 (abcde!!1)), (abcde!!2), abcde!!3]




//t
intsToHexStr :: [Int] -> String
intsToHexStr [] 		= ""
intsToHexStr [i:is] 	= (intToHexStr i) +++ (intsToHexStr is)
//t
intToHexStr :: Int -> String
intToHexStr i	= (byteToHexStr ((i bitand 0xff000000)>>24)) +++ (byteToHexStr ((i bitand 0x00ff0000)>>16)) +++ (byteToHexStr ((i bitand 0x0000ff00)>>8)) +++ (byteToHexStr (i bitand 0x000000ff))
where
	byteToHexStr :: Int -> String
	byteToHexStr i = ((toString h.[i/16]) +++ (toString h.[(i rem 16)]))
	where
		h = "0123456789abcdef"//{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'}

extIter :: Int [Int] -> [Int]
extIter 80 x = x
extIter n x = extIter (n+1) (x ++ [leftrotate32 1 ((x!!(n-3)) bitxor (x!!(n-8)) bitxor (x!!(n-14)) bitxor (x!!(n-16)))])



//t
// vector sum of elements modulo 2**32
vSum :: [Int] [Int] -> [Int]
vSum [] _ 			= []
vSum _ []			= []
vSum [a:as] [b:bs]	= [(a + b) bitand 0xffffffff:(vSum as bs)]


sha1 :: String -> [Int]
sha1 s = process512Chunk hi (chunck512WithSize s)
where
	hi = [0x67452301,0xEFCDAB89, 0x98BADCFE, 0x10325476,0xC3D2E1F0]



//chunck512WithSize ""

Start = intsToHexStr (sha1 "Subsequently, on 12 August 2004, a collision for the full SHA-0 algorithm was announced by Joux, Carribault, Lemuet, and Jalby. This was done by using a generalization of the Chabaud and Joux attack. Finding the collision had complexity 251 and took about 80,000 CPU hours on a supercomputer with 256 Itanium 2 processors. (Equivalent to 13 days of full-time use of the computer.)")