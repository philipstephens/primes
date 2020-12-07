module GenPrimes where

prime_list = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,247,251,257,263,269,271,277,281,283,293,299,307,311,313,317,323,331,337,347,349,353,359,367,373,379,383,389,391,397,401,403,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,527,541,547,557,559,563,569,571,577,587,593,599,601,607,611,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,731,733,739,743,751,757,761,767,769,773,787,797,799,809,811,821,823,827,829,839,853,857,859,863,871,877,881,883,887,899,907,911,919,923,929,937,941,947,953,967,971,977,983,991,997,1003]

prime3_list = [3,7,11,19,23,31,43,47,59,67,71,79,83,103,107,127,131,139,151,163,167,179,191,199,211,223,227,239,251,263,271,283,307,311,331,347,359,367,379,383,419,431,439,443,463,467,479,487,491,499,503,523,547,563,571,587,599,607,619,631,643,647,659,683,691,719,727,739,743,751,787,811,823,827,839,859,863,883,887,907,911,919,947,967,971,983,991]


-- def: prime1 = every prime number of the form 4 * k + 1
--               where k >= 1
--
--      prime3 = every prime number of the form 4 * k + 3
--               where k >= 0
--
--      non-prime1 = every prime1 candidate that is not prime1
--      non-prime3 = every prime3 candidate that is not prime3
--
--      candidate = every number of the form 4*k+1 or 4*k+3
--
-- Every prime number can be expressed as a prime1 or prime3 number
--
-- Note that not every candidate (4*k+1) or (4*k+3) is prime 
--
-- It is my observation (belief) that every non-prime3 number is divisible
-- by a prime3 number.
--
-- Every non-prime1 number is divisible by a prime1 or a prime3 number.
--

isPrime' :: Integer -> [Integer] -> Bool
isPrime' _  [] = True
isPrime' p (x:xs)
  | p == 2                                    = True
  | p == 3                                    = True
  | x > round (sqrt (fromIntegral p))         = True
  | p `mod` x == 0                            = False
  | otherwise                                 = isPrime' p xs

isTypeP3 :: Integer -> Bool
isTypeP3 x = if (x-3) `mod` 4 == 0 then True else False

isTypeP1 :: Integer -> Bool
isTypeP1 x = if (x-1) `mod` 4 == 0 then True else False

isPrime :: Integer -> Bool
isPrime p 
    | isTypeP3 p = isPrime' p prime3_list
    | isTypeP1 p = isPrime' p prime_list
    | otherwise  = False

findfirstfactor :: Integer -> Integer -> Integer
findfirstfactor p counter
   | (p <= 0)                                  = -1
   | counter <= 1                              = -1
   | (p `mod` counter == 0)                    = counter
   | (counter > round (sqrt (fromIntegral p))) = 1
   | otherwise                                 = findfirstfactor p (counter+1)
     
-- 1 means prime
nonPrimeFactor :: Integer -> Integer
nonPrimeFactor p
    | isPrime  p = 1
    | otherwise  = findfirstfactor p 2  

genRawFilter1 :: Integer -> [Integer]
genRawFilter1 len = [ 4 * k + 1 | k <- [1..len], isPrime' (k*4+1) prime_list]

genRawFilter3 :: Integer -> [Integer]
genRawFilter3 len = [ 4 * k + 3 | k <- [1..len], isPrime' (k*4+3) prime3_list]

rawFilter1 = genRawFilter1 250
rawFilter3 = 2:3:genRawFilter3 250

cleanFilter3 :: [Integer] -> [Integer]
cleanFilter3 [] = []
cleanFilter3 (x:xs) = if x         == 3 then x:cleanFilter3 xs else
                      if x         == 7 then x:cleanFilter3 xs else
                      if x `mod` 3 == 0 then cleanFilter3 xs else
                      if x `mod` 5 == 0 then cleanFilter3 xs else
                      if x `mod` 7 == 0 then cleanFilter3 xs else
                      if not (isPrime' x rawFilter3) then cleanFilter3 xs
                      else x:cleanFilter3 xs

cleanFilter1 :: [Integer] -> [Integer]
cleanFilter1 [] = []
cleanFilter1 (x:xs) = if x         == 5 then x:cleanFilter1 xs else
                      if x `mod` 3 == 0 then cleanFilter1 xs else
                      if x `mod` 5 == 0 then cleanFilter1 xs else
                      if x `mod` 7 == 0 then cleanFilter1 xs else
                      if not (isPrime' x rawFilter3) && 
                         not (isPrime' x rawFilter1) then cleanFilter1 xs
                        else x:cleanFilter1 xs 

cleanList1 = cleanFilter1 rawFilter1
cleanList3 = cleanFilter3 rawFilter3

combPrimes :: [Integer] -> [Integer] -> [Integer]
combPrimes xs [] = xs
combPrimes [] ys = ys
combPrimes (x:xs) (y:ys) = if (x < y) then x : combPrimes xs (y:ys) else
  y : combPrimes (x:xs) ys

divP1 :: Integer -> [Integer]
divP1 len = [ 4*k+1 | k <-[1..len], isPrime' (k*4+1) prime_list]

divP3 :: Integer -> [Integer]
divP3 len = [ 4*k+3 | k <-[0..len], isPrime' (k*4+3) prime3_list]
