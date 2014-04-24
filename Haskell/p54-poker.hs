import Data.List
data Suit = D | C | S | H
type Rank = Int
data Card = Card Suit Rank

data Hand = Hand Card Card Card Card Card
instance Ord Hand where

data PokerHand = StraightFlush Rank
               | FourKind      Rank
               | FullHouse     Rank
               | Flush         Rank
               | Straight      Rank
               | ThreeKind     Rank Rank Rank
               | TwoPair       Rank Rank Rank
               | Pair          Rank Rank Rank Rank
               | HighCard      Rank Rank Rank Rank Rank


getSuit :: Card -> Suit
getSuit (Card s _) = s

getRank :: Card -> Int
getRank (Card _ r) = r

getRanks :: Hand -> [Rank]
getRanks (Hand c1 c2 c3 c4 c5) = map getRank [c1, c2, c3, c4, c5]

getSuits :: Hand -> [Suit]
getSuits (Hand c1 c2 c3 c4 c5) = map getSuit [c1, c2, c3, c4, c5]

-- parseHand :: String -> Hand
-- parseHand s = h where

parseStraight :: Hand -> Maybe PokerHand
parseStraight h = if isStraight then Just $ Straight (last sorted) else Nothing
  where
    isStraight = [0..4] == map (+ (-smallest)) sorted
    sorted@(smallest:_) = sort (getRanks h)

parseFlush :: Hand -> Maybe PokerHand
parseFlush h = if isFlush h then Just $ Flush (maximum (getRanks h)) else Nothing
where
  isFlush = (== 1) . length . nub . getSuits

parseStraightFlush :: Hand -> Maybe PokerHand
parseStraightFlush h = case (parseStraight h, parseFlush h) of
  (Just c, Just _) -> Just $ StraightFlush c
  _                -> Nothing

getRankCounts :: Hand -> [(Int, Rank)]
getRankCounts = getCounts . getRanks

getHandCounts :: Hand -> [Int]
getHandCounts = map fst . getRankCounts

parseFourKind :: Hand -> Maybe PokerHand
parseFourKind h = if match h then Just FourKind biggest h else Nothing
where
  match = (== 4) . fst . head . getRankCounts
  biggest = snd . head . getRankCounts

parseFullHouse :: Hand -> Maybe PokerHand
parseFullHouse h = if match h then Just FullHouse biggest secondBiggest else Nothing
where
  match = (== [3, 2]) . map fst . getRankCounts
  biggest =       snd $ head   $ getRankCounts $ h
  secondBiggest = snd $ (!! 1) $ getRankCounts $ h

parseThreeKind :: Hand -> Maybe PokerHand
parseThreeKind h = if match h then Just ThreeKind a b c else Nothing
where
  match = (== [3, 2])

getCounts :: Ord a => [a] -> [(Int, a)]
getCounts xs = reverse . sort (foldr incrementCountList [] xs) where
  incrementCountList :: Eq a => a -> [(a, Int)] -> [(a, Int)]
  incrementCountList x [] = [(0, x)]
  incrementCountList x ((c, e) : cs)
    | e == x = (c+1, e) : cs
    | otherwise = (c, e) : (incrementCountList cs)


hand2PokerHand :: Hand -> PokerHand
hand2PokerHand h = (getCons h) ranksSortedByCountThenRank where
  countRankPairs = getCounts . getRanks h
  counts = map fst countRankPairs
  ranksSortedByCountThenRank = map snd countRankPairs
  getCons :: Hand -> ([Int] -> PokerHand)
  getCons h
    | (isStraight h) && (isFlush h) = StraightFlush
    | isStraight h                  = Straight
    | isFlush h                     = Flush
    | counts == [4, 1]              = FourKind
    | counts == [3, 2]              = FullHouse
    | counts == [3, 1, 1]           = ThreeKind
    | counts == [2, 2, 1]           = TwoPair
    | counts == [2, 1, 1, 1]        = Pair
    | otherwise                     = HighCard


isStraightFlush :: Hand -> Maybe PokerHand


main :: IO ()
main = print "hello"
