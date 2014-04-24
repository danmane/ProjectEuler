import Control.Arrow
import Control.Monad


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

getCounts :: (Eq a, Ord a) => [a] -> [(Int, a)]
getCounts = sort . map (length &&& head) . group . sort

getRanks :: Hand -> [Rank]
getRanks (Hand c1 c2 c3 c4 c5) = map getRank [c1, c2, c3, c4, c5] where
  getRank (Card _ r) = r

getSuits :: Hand -> [Suit]
getSuits (Hand c1 c2 c3 c4 c5) = map getSuit [c1, c2, c3, c4, c5] where
  getSuit (Card s _) = s

getRankCounts :: Hand -> [(Int, Rank)]
getRankCounts = getCounts . getRanks

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
    [(2, r1), (1, r2), (1, r3), (1 r4)]           -> Pair      r1 r2 r3
    [(1, r1), (1, r2), (1, r3), (1, r4), (1, r5)] -> HighCard  r1 r2 r3 r4 r5
  where
    isStraight = (== [0..4]) . uncurry map . (flip (-) . head &&& id)
    sortedRanks = sort $ getRanks h
    maxRank = last sortedRanks
    isFlush = (== 1) . length . nub . getSuits

rankify :: PokerHand -> [Rank]
rankify ph
  | StraightFlush r1             = [9, r1]
  | FourKind      r1             = [8, r1]
  | FullHouse     r1 r2          = [7, r1, r2]
  | Flush         r1             = [6, r1]
  | Straight      r1             = [5, r1]
  | ThreeKind     r1 r2 r3       = [4, r1, r2, r3]
  | TwoPair       r1 r2 r3       = [3, r1, r2, r3]
  | Pair          r1 r2 r3 r4    = [2, r1, r2, r3, r4]
  | HighCard      r1 r2 r3 r4 r5 = [1, r1, r2, r3, r4, r5]

main :: IO ()
main = print "hello"
