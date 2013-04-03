string2IntGrid :: String -> [[Int]]
string2IntGrid = map (map read) . (map words . lines)

triPlus :: [Int] -> [Int] -> [Int]
triPlus [] _ = []
triPlus (s:ss) (l1 : l2 : ls) = s + max l1 l2 : triPlus ss (l2 : ls)

triMax :: [[Int]] -> Int
triMax tri = head $ foldr triPlus (last tri) (init tri)

solve = (++"\n") . show . triMax . string2IntGrid

main = interact solve


--   1
--  9 3
-- 1 2 3
--4 5 6 7
t1_triPlus = [1,2,3] `triPlus` [4,5,6,7]  == [6, 8, 10]
t2_triPlus = [9, 3]  `triPlus` [6, 8, 10] == [17, 13]
t3_triPlus = [1]     `triPlus` [17, 13]   == [18]
t_triMax = triMax [ [1], [9,3], [1,2,3], [4,5,6,7]] == 18
