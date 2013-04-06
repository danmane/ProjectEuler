import Data.List
import qualified Data.Set as Set


primes :: [Int]
primes = 2 : 3 : primesR [2,3] 5 where
	primesR li n = case divisible n li of 
		True  -> primesR li (n+2)
		False -> n : primesR (n:li) (n+2)

divisible :: Int -> [Int] -> Bool
divisible x li = any (== 0) $ map (mod x) li

diagonals :: Int -> [Int]
diagonals i = 1 : diagonalsR 1 2 (i-1) where
	diagonalsR _ _ 0 = []
	diagonalsR prev step remain = foldr (:) (diagonalsR (prev + step * 4) (step+2) (remain-1)) newVals where
		newVals = map (\x -> prev + step * x) [1,2,3,4]

percentPrime :: [Int] -> Float
percentPrime = 