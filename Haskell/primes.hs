import Data.List
import qualified Data.Set as Set


primes :: [Int]
primes = 2 : 3 : primesR [2,3] 5 where
	primesR li n = case divisible n li of 
		True  -> primesR li (n+2)
		False -> n : primesR (n:li) (n+2)

divisible :: Int -> [Int] -> Bool
divisible x li = any (== 0) $ map (mod x) li

