implementation module Random


import	StdInt, StdClass
import	StdTime
import StdTuple
import StdList

:: RandomSeed	:== Int
:: RandSeed :== [Int] // Four 32-bit integers

nullRandomSeed :: RandomSeed
nullRandomSeed
	=	0
	
e1 :: [a] -> a
//e1 [] 		= abort "too short"
e1 [e:es] 	= e

e2 :: [a] -> a
//e1 [] 		= abort "too short"
e2 [_:e:es] 	= e

e3 :: [a] -> a
//e1 [] 		= abort "too short"
e3 [_:_:e:es] 	= e

e4 :: [a] -> a
//e1 [] 		= abort "too short"
e4 [_:_:_:e:es] 	= e

getNewRandomSeed :: !*env -> (!RandomSeed, !*env) | TimeEnv env
getNewRandomSeed env
	# ({hours,minutes,seconds}, env)	= getCurrentTime env
	= (1+(hours+minutes+seconds) bitand 65535, env)

random :: !RandomSeed -> (!Int,!RandomSeed)
random seed
	=	(newSeed,newSeed)
where
	newSeed		= if (nextSeed>=0) nextSeed (nextSeed+65537)
	nextSeed	= (seed75 bitand 65535)-(seed75>>16)
	seed75		= seed*75

//generates seed using congruent PRNG 
getFourRandom :: !RandomSeed Int -> [Int]
getFourRandom _ 0 	= []
getFourRandom rs n 	= ([((fst rand) << 16) + (fst rand1)] ++ (getFourRandom (snd rand1) (n-1)))
where
	rand 	= (random rs)
	rand1	= random (snd rand) 

getNewRandSeed :: !*env -> (!RandSeed, !*env) | TimeEnv env
getNewRandSeed env
	# ({hours,minutes,seconds}, env)	= getCurrentTime env
	# (rs, env)							= (1+(hours+minutes+seconds) bitand 65535, env)
	=	(getFourRandom rs 4, env)
	

randomXorShift :: !RandSeed -> (!Int,!RandSeed)
randomXorShift randSeed
	# (t,randSeed) 		= ((((e1 randSeed) << 11) bitand 0xffffffff) bitxor (e1 randSeed), randSeed)
	# (w, randSeed)		= (((e4 randSeed) bitxor ((e4 randSeed) >> 19) bitxor t bitxor (t >> 8)),randSeed)
	# (w,randSeed)		= (w, [(e2 randSeed), (e3 randSeed), (e4 randSeed), w])
	= (w, randSeed)
	
	
randomXSN :: Int !RandSeed -> [Int]
randomXSN 0 randSeed = []
randomXSN n randSeed = [(fst r):(randomXSN (n-1) (snd r))]
where
	r = randomXorShift randSeed
	
Start world = drop 9999999 (randomXSN 10000000 rs)
where
	rs = (fst (getNewRandSeed world))