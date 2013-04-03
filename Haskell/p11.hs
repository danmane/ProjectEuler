import Data.Array.IArray
import Data.List

--parseGridString :: String -> Array Int Int

string2Grid :: String -> [[String]]
-- convert a string like "1 2 3 \n 2 3 4" to [["1","2","3"],["2","3","4"]]
string2Grid = map words . lines

string2IntGrid :: String -> [[Int]]
string2IntGrid = map (map read) . string2Grid

intGrid2Array :: [[Int]] -> Array (Int, Int) Int
intGrid2Array xxs = array bounds vals where
	bounds = ((0, 0), (nRows-1, nCols-1))
	nRows = length xxs
	nCols = length (head xxs)
	vals = [(x, xxs !! fst x !! snd x ) | x <- range bounds]

--gridString2Ints :: String -> [[Int]]
--gridString2Ints = (map read) . (map words) . lines

--intList2Array :: [[Int]] -> Array Int Int

getP11Ans :: String -> Int
getP11Ans s = maximum $ map product valseqs where
	intGrid :: [[Int]]
	intGrid = string2IntGrid s -- this will be a list of lists, where each sublist is 
	-- represents a row
	nRows :: Int
	nRows = length intGrid
	nCols :: Int
	nCols = (length . head) intGrid
	intArray :: Array (Int, Int) Int
	intArray = intGrid2Array intGrid -- an array (with O(1) reference)
	sequences :: [[(Int, Int)]]
	sequences = allIdxSequences (nRows, nCols) 4

	valseqs :: [[Int]]
	valseqs = map (map (intArray ! )) sequences


tPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
tPlus (a,b) (c,d) = (a+c, b+d)

getAdjIndicies :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- variables: nRemaining, coordinate, step size
getAdjIndicies 0 _ _ = []
getAdjIndicies nRemaining step coord = coord : getAdjIndicies (nRemaining - 1) step (coord `tPlus` step)  

allIdxSequences :: (Int, Int) -> Int -> [[(Int, Int)]]
-- takes the (xBound, yBound) of the array and the size of each sequence. returns a list containing every list of index subsequence
allIdxSequences (nRows, nCols) size = filter (all inBounds) seqs where
	coords = [(x, y) | x <- [0..nRows-1], y <- [0..nCols-1]]
	directions = [(0,1), (1,0), (1,1), (1, -1)]
	seqs = [getAdjIndicies size direction coord | direction <- directions, coord <- coords]
	inBounds (x,y) = (x >= 0) && (x < nRows) && (y >= 0) && (y < nCols)

main = do 
	gridStr <- readFile "p11_grid.txt"
	let ans = getP11Ans gridStr
	putStrLn $ show ans


teststr = "1 2 3 \n 2 3 4"
teststr2 = "1 2 3\n 2 3 4\n 4 5 9"

test_string2Grid = string2Grid teststr == expected where
	expected = [["1","2","3"],["2","3","4"]]

test_string2IntGrid = string2IntGrid teststr == expected where
	expected = [[1,2,3],[2,3,4]] :: [[Int]]

t_getAdjIndicies = getAdjIndicies 4 (1,1) (0,0) == [(0,0), (1,1), (2,2), (3,3)]

t_allIdxSequences = t1 && t2 where
	t1 = nub (allIdxSequences (3, 3) 1) == [[(a,b)] | a <- [0..2], b <- [0..2]]
	pts2x2 = [(a,b) | a <- [0,1], b <- [0,1]]
	expected = sort [[pts2x2 !! a, pts2x2 !! b] | a <- [0..2], b <- [a+1..3] ]
	actual = sort (allIdxSequences (2, 2) 2)
	t2 = expected == actual

test = and [test_string2Grid, test_string2IntGrid, t_getAdjIndicies, t_allIdxSequences]