import Data.List
import Primes

oddComposites = [3,5..] \\ primes
doubleSquares = map ((*2) . (^2)) [1..]

goldbachPredicate n = n `elem` [p + 2 * s | p <- filter (<n) primes, s <- filter (<n) doubleSquares]

main = print $ find (not . goldbachPredicate) oddComposites
--(print $ take 20 primes) >> (print $ take 20 naivePrimes)
