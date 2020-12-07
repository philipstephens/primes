module Main where

import GenPrimes

main :: IO ()
main = do
  putStrLn "Prime Numbers:\n"
  print $ combPrimes (cleanFilter3 (genRawFilter3 250))
                     (cleanFilter1 (genRawFilter1 250))

  putStr "\n\nIs 4387 a prime number ? "
  if (isPrime 4387) == True then print "Yes" else print "No"

