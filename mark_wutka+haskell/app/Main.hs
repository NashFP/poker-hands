module Main where

import Poker

getResult l =
  let (h1,h2) = readHands l in
    l ++ "  " ++ show (determineWinner h1 h2)
  
main :: IO ()
main = do
  contents <- readFile "../data/p054_poker.txt"
  let hands = lines contents
  let results = map getResult hands
  putStrLn $ unlines results
  
