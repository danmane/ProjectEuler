getDivisors :: Int -> [Int]
getDivisors n = [y | y <- [1..(x `div` 2)], mod x y == 0]



isAbundant :: Int -> Bool
isAbundant x = (sum $ getDivisors x) > x

smallNum = 28123

smallAbundants :: [Int]
smallAbundants = filter isAbundant [1..28123]