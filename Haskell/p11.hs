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
getP11Ans s = 1 where
	intGrid = string2IntGrid s -- this will be a list of lists, where each sublist is 
	-- represents a row
	maxRows = maximum $ map maxFourProduct intGrid -- map the maxFourProduct over each row
	maxCols = maximum $ map maxFourProduct $ transpose intGrid -- map the maxFourProduct over columns
	intArray = intGrid2Array intGrid -- an array (with O(1) reference)
	getDiagIdx

tTimes :: (Int, Int) -> Int -> (Int, Int)
tTimes (a,b) x = (a*x, b*x)

tPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
tPlus (a,b) (c,d) = (a+c, b+d)

diagonalize :: (Int, Int) -> Int -> (Int, Int) -> [(Int, Int)]
diagonalize step nSteps startCoord = [startCoord `tPlus` (step `tTimes` n) | n <- [0..nSteps-1]]

getDiagonalIndicies :: (Int, Int) -> Int -> [[(Int, Int)]]
-- takes the bounds of the array (assumed to use 0-based indexing)
-- also takes the direction of the x step (+1 corresponds to going right, 
-- -1 corresponds to going left)
-- then returns a list of lists of coordinate tuples, representing each list of 
-- diagonal indices in the array
getDiagonalIndicies (xBound, yBound) xStep = map (filter inBounds) ds where
	len = max xBound yBound
	s1 = case xStep of
		1  -> -len
		-1 -> 0
		_  -> error "Use 1 or -1 as xStep"
	s2 = case xStep of
		1  -> len
		-1 -> 2*len
		_  -> error "Use 1 or -1 as xStep"
	start = [(0, c) | c <- [s1, s2]]
	ds = map (diagonalize (1, xStep) len) start
	inBounds (a,b) = (a>)


maxFourProduct :: [Int] -> Int
maxFourProduct = maximum . fourProduct

fourProduct :: [Int] -> [Int]
-- take a list of integers and return a list of every # multiplied by the three previous numbers
fourProduct (a:b:c:xs) = fourProductR xs a b c where
	fourProductR :: [Int] -> Int -> Int -> Int -> [Int]
	-- keeps the 3 previous ints as function arguments 
	fourProductR [] _ _ _ = []
	fourProductR (x:xs) a b c = x * a * b * c : fourProductR xs x a b
fourProduct _ = []



--testE = [((i,j), i + j) | i <- [0..3], j <- [0..3]]

--testI = ( (0,0), (3,3) )

--testA = array testI testE
teststr = "1 2 3 \n 2 3 4"

test_string2Grid = string2Grid teststr == expected where
	expected = [["1","2","3"],["2","3","4"]]

test_string2IntGrid = string2IntGrid teststr == expected where
	expected = [[1,2,3],[2,3,4]] :: [[Int]]

test_fourProduct = t1 && t2 where 
	t1 = fourProduct [1,1,1,1,2,2,2,2] == [1,2,4,8,16] 
	t2 = fourProduct [1,2,3] == []

t_diagonalize = t1 && t2 where
	t1 = diagonalize (0,0) (1,1)  5 == [(0,0), (1,1), (2,2), (3,3), (4,4)]
	t2 = diagonalize (0,5) (1,-1) 6 == [(0,5), (1,4), (2,3), (3,2), (4,1), (5,0)]

test = and [test_string2Grid, test_string2IntGrid, test_fourProduct, t_diagonalize]