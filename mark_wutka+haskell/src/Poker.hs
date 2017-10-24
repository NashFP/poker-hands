module Poker where

import Data.Char
import Data.List

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq)

data CardType = Card Int Suit deriving (Eq)

data WinnerType = Hand1 | Hand2 | Tie deriving (Show)

-- Define all the I/O routines for reading and displaying cards

showRank :: Int -> String
showRank i
  | i >= 2 && i <= 9 = (show i)
  | i == 10 = "T"
  | i == 11 = "J"
  | i == 12 = "Q"
  | i == 13 = "K"
  | i == 14 = "A"
  | True = error "Invalid rank"

instance Show Suit where
  show Clubs = "C"
  show Diamonds = "D"
  show Hearts = "H"
  show Spades = "S"
  
instance Show CardType where
  show (Card rank suit) = (showRank rank) ++ (show suit)

readSuit :: Char -> Suit
readSuit 'C' = Clubs
readSuit 'D' = Diamonds
readSuit 'H' = Hearts
readSuit 'S' = Spades
readSuit s = error ("Invalid suit "++[s])

readRank :: Char -> Int
readRank r
  | r >= '2' && r <= '9' = ord(r) - ord('0')
  | r == 'T' = 10
  | r == 'J' = 11
  | r == 'Q' = 12
  | r == 'K' = 13
  | r == 'A' = 14
  | True = error ("Invalid rank" ++ [r])
  
readCard [r,s] = Card (readRank r) (readSuit s)

readHands l =
  (take 5 cards, drop 5 cards)
  where
    cards = map readCard (words l)


-- Utities for arranging cards

compareCards (Card r1 _) (Card r2 _) =
  compare r2 r1

sortCards =
  sortBy compareCards

rank (Card r _) = r
suit (Card _ s) = s

sameSuit (Card _ s1) (Card _ s2) = s1 == s2
sameRank (Card r1 _) (Card r2 _) = r1 == r2

allSameRank [] = True
allSameRank [_] = True
allSameRank (c1:c2:rest) =
  if r1 == r2 then
    allSameRank (c2:rest)
  else
    False
  where
    r1 = rank c1
    r2 = rank c2
    
isFlush h =
  all (sameSuit (head h)) h

isStraight [] = True
isStraight [_] = True
isStraight (c1:c2:rest) =
  if r1 == r2+1 then
    isStraight (c2:rest)
  else
    False
  where
    r1 = rank c1
    r2 = rank c2
    
royalFlush :: [CardType] -> (Bool, [CardType])
royalFlush h =
  (sResult && rank (head sTie) == 14, sTie)
  where
    (sResult, sTie) = straightFlush h

straightFlush :: [CardType] -> (Bool, [CardType])
straightFlush h =
  (isFlush h && sResult, sTie)
  where
    (sResult, sTie) = straight h

fourOfAKind :: [CardType] -> (Bool, [CardType])
fourOfAKind h =
  if allSameRank (take 4 hSorted) then
    (True, [last hSorted])
  else if allSameRank (drop 1 hSorted) then
    (True, [head hSorted])
       else
         (False, [])
  where
    hSorted = sortCards h

fullHouse :: [CardType] -> (Bool, [CardType])
fullHouse h =
  if allSameRank (take 3 hSorted) && allSameRank (drop 3 hSorted) then
    (True, [head hSorted, head (drop 3 hSorted)])
  else if (allSameRank (take 2 hSorted)) && (allSameRank (drop 2 hSorted)) then
    (True, [head (drop 2 hSorted), head hSorted])
       else
         (False,[])
  where
    hSorted = sortCards h

flush :: [CardType] -> (Bool, [CardType])
flush h =
  (isFlush h, sortCards h)

straight :: [CardType] -> (Bool, [CardType])
straight h =
  if isStraight hSorted then
    (True, hSorted)
  else
    (False, [])
  where
    hSorted = sortCards h
    

threeOfAKind :: [CardType] -> (Bool, [CardType])
threeOfAKind h =
  if allSameRank (take 3 hSorted) then
    (True, (head hSorted) : sortCards (drop 3 hSorted))
  else if allSameRank (take 3 (tail hSorted)) then
    (True, (head (tail hSorted)) : sortCards [head hSorted, last hSorted])
  else if allSameRank (drop 2 hSorted) then
    (True, (last hSorted) : sortCards (take 2 hSorted))
       else
         (False,[])
  where
    hSorted = sortCards h

twoPair :: [CardType] -> (Bool, [CardType])
twoPair h =
  if allSameRank (take 2 hSorted) then
    if allSameRank (take 2 (drop 2 hSorted)) then
      (True, (sortCards [head hSorted, head (drop 2 hSorted)])++[last hSorted])
    else if allSameRank (drop 3 hSorted) then
      (True, (sortCards [head hSorted, last hSorted])++[head (drop 2 hSorted)])
    else
      (False, [])
  else if allSameRank (take 2 (tail hSorted)) &&
          allSameRank (drop 3 hSorted) then
         (True, (sortCards [head (tail hSorted), head (drop 3 hSorted)])++[head hSorted])
       else
         (False, [])
  where
    hSorted = sortCards h

onePair :: [CardType] -> (Bool, [CardType])
onePair h =
  if length nubList == 4 then
    let paired = head (h \\ nubList) in
      (True, paired : (sortCards (filter (\c -> (rank c) /= rank paired) h)))
  else
    (False, [])
  where
    nubList = nubBy sameRank h
    
highCard :: [CardType] -> (Bool, [CardType])
highCard h = (True, sortCards h)

breakTie [] [] = Tie
breakTie (c1:rest1) (c2:rest2) =
  if rank c1 > rank c2 then
    Hand1
  else if rank c2 > rank c1 then
    Hand2
       else
         breakTie rest1 rest2

handRanking = [royalFlush, straightFlush, fourOfAKind, fullHouse, flush, straight,
               threeOfAKind, twoPair, onePair, highCard]

compareHands [] h1 h2 = Tie
compareHands (compFunc:compRest) h1 h2 =
  if matchH1 then
    if matchH2 then
      breakTie h1TieBreak h2TieBreak
    else
      Hand1
  else if matchH2 then
    Hand2
       else
         compareHands compRest h1 h2
  where
    (matchH1, h1TieBreak) = compFunc h1
    (matchH2, h2TieBreak) = compFunc h2
  
determineWinner h1 h2 =
  compareHands handRanking h1 h2
