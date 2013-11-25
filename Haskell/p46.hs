import Data.List

isPrime :: [Int] -> Int -> Bool
isPrime plist n = all (\x -> n `mod` x /= 0) smallPrimes
    where smallPrimes = takeWhile (\x -> x * x <= n) plist

potentialPrimes = [6 * x + y | x <-[1..], y <- [-1,1]]
primeList = 2:3:filter (isPrime primeList) potentialPrimes

isPrimeNaive :: Int -> Bool
isPrimeNaive n = all (/= 0)  (map (mod n) [2..n-1])
naivePrimes = 2 : filter isPrimeNaive [3..]

oddComposites = [3,5..] \\ primeList
doubleSquares = map ((*2) . (^2)) [1..]

goldbachPredicate n = n `elem` [p + 2 * s | p <- filter (<n) primeList, s <- filter (<n) doubleSquares]

main = print $ find (not . goldbachPredicate) oddComposites
--(print $ take 20 primeList) >> (print $ take 20 naivePrimes)
