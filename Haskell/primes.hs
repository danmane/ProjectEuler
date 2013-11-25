module Primes (primes) where
isPrime :: [Int] -> Int -> Bool
isPrime plist n = all (\x -> n `mod` x /= 0) smallPrimes
    where smallPrimes = takeWhile (\x -> x * x <= n) plist

potentialPrimes = [6 * x + y | x <-[1..], y <- [-1,1]]
primes = 2:3:filter (isPrime primes) potentialPrimes

isPrimeNaive :: Int -> Bool
isPrimeNaive n = all (/= 0)  (map (mod n) [2..n-1])
naivePrimes = 2 : filter isPrimeNaive [3..]
