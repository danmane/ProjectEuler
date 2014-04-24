import Control.Arrow
import Control.Monad
import Data.Char

import Data.List
data Suit = D | C | S | H deriving (Eq, Read, Show, Enum)
type Rank = Int
data Card = Card Rank Suit deriving (Eq, Show)
data Player = Player1 | Player2 deriving Eq

data Hand = Hand Card Card Card Card Card deriving Eq
instance Ord Hand where
  h1 `compare` h2 = (parse h1) `compare` (parse h2)

data PokerHand = HighCard      Rank Rank Rank Rank Rank
               | Pair          Rank Rank Rank Rank
               | TwoPair       Rank Rank Rank
               | ThreeKind     Rank Rank Rank
               | Straight      Rank
               | Flush         Rank
               | FullHouse     Rank
               | FourKind      Rank
               | StraightFlush Rank
                 deriving (Eq, Ord)

getCounts :: (Eq a, Ord a) => [a] -> [(Int, a)]
getCounts = reverse .sort . map (length &&& head) . group . sort

parse :: Hand -> PokerHand
parse h
  | (isStraight sortedRanks) && isFlush h          = StraightFlush maxRank
  | isFlush h                                      = Flush         maxRank
  | isStraight sortedRanks                         = Straight      maxRank
  | otherwise = case getRankCounts h of
    [(4, r), (1, _)]                              -> FourKind  r
    [(3, r), (2, _)]                              -> FullHouse r
    [(3, r1), (1, r2), (1, r3)]                   -> ThreeKind r1 r2 r3
    [(2, r1), (2, r2), (1, r3)]                   -> TwoPair   r1 r2 r3
    [(2, r1), (1, r2), (1, r3), (1, r4)]          -> Pair      r1 r2 r3 r4
    [(1, r1), (1, r2), (1, r3), (1, r4), (1, r5)] -> HighCard  r1 r2 r3 r4 r5
  where
    isStraight = (== [0..4]) . uncurry map . (flip (-) . head &&& id)
    sortedRanks = sort $ getRanks h
    maxRank = last sortedRanks
    isFlush = (== 1) . length . nub . getSuits
    getSuits (Hand c1 c2 c3 c4 c5) = map getSuit [c1, c2, c3, c4, c5]
    getSuit (Card _ s) = s
    getRanks (Hand c1 c2 c3 c4 c5) = map getRank [c1, c2, c3, c4, c5]
    getRank (Card r _) = r
    getRankCounts = getCounts . getRanks

parseRank :: Char -> Rank
parseRank c = case c of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 11
  'T' -> 10
  _ -> digitToInt c

string2Winner :: String -> Player
string2Winner s = if (fst hands > snd hands) then Player1 else Player2 where
  hands = string2Hands s

string2Card :: String -> Card
string2Card s = Card (parseRank $ head s) (read $ tail s)

string2Cards :: String -> ([Card], [Card])
string2Cards = splitAt 5 . map string2Card . words

string2Hands :: String -> (Hand, Hand)
string2Hands = applyTuple c2h . string2Cards

applyTuple :: (a -> b) -> (a, a) -> (b, b)
applyTuple f (a1, a2) = (f a1, f a2)

c2h :: [Card] -> Hand
c2h [c1, c2, c3, c4, c5] = Hand c1 c2 c3 c4 c5

main :: IO ()
main = do
  theString <- readFile "hands.txt"
  let result = length $ filter (== Player1) $ map string2Winner $ lines $ theString
  print result
