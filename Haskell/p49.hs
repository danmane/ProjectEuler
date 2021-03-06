import Data.List
import qualified Data.Set as Set

smallLimit = 10000

primes :: [Int]
primes = 2 : 3 : primesR [2,3] 5 where
	primesR li n = case divisible n li of 
		True  -> primesR li (n+2)
		False -> n : primesR (n:li) (n+2)

divisible :: Int -> [Int] -> Bool
divisible x li = any (== 0) $ map (mod x) li

smallPrimes = takeWhile (<smallLimit) primes
smallPrimeSet = Set.fromList smallPrimes


fourDigitPrimes = filter (>1000) smallPrimes

getDigits :: Int -> [Int]
getDigits x = lsd : (if remainder == 0 then [] else getDigits remainder) where
	remainder = (x - lsd) `div` 10
	lsd = x `mod` 10

allDigitCombos :: [[Int]]
allDigitCombos = nub $ map sort $ map getDigits fourDigitPrimes

permutables :: [[Int]]
permutables = [filter ((==x) . sort . getDigits) fourDigitPrimes | x <- allDigitCombos]

shortPermutables = filter ((>3) . length) permutables

arithSeqs :: [Int] -> [(Int, Int, Int)]
arithSeqs (x:xs) = [(x, y, 2*y-x) | y <- xs, 2*y-x `elem` xs] ++ arithSeqs xs
arithSeqs [] = []
-- arithSeqs [1,3,5,6,7,11] = [(1,3,5), (3,5,7), (1,6,11), (5,6,7)]

p49 = foldr (++) [] $ map arithSeqs shortPermutables
