{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Wordle.Solver (solve, guessWord', Letter (..), Guess) where

import Control.Lens
import Data.Char
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data Letter
  = Correct Char Int
  | Contains Char Int
  | Incorrect Char
  | NoAnswer
  deriving (Show)

type Guess = [Letter]

type Dictionary = [Text]

applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

hasLetter :: Char -> Text -> Bool
hasLetter c t = isJust (T.find (== c) t)

reduceL :: Letter -> Dictionary -> Dictionary
reduceL l d =
  case l of
    Correct c i -> filter (\x -> T.index x i == c) d
    Contains c i -> (filter (\x -> T.index x i /= c) . filter (hasLetter c)) d
    Incorrect c -> filter (not . hasLetter c) d
    NoAnswer -> d

reduceG :: Guess -> Dictionary -> Dictionary
reduceG g = applyAll (map reduceL g)

balancedChar :: Dictionary -> [Char]
balancedChar d = map fst (sortOn (Down . snd) (M.toList counts))
  where
    l = length d
    f v w = foldl' (\x c -> x & at c . non 0 %~ (+ 1)) v (nub (T.unpack w))
    counts = M.filter (/= 0) (foldl' f M.empty d & traverse %~ triangle)
    triangle x = let m = l `div` 2 in if x >= m then l - x else x

guessWord :: Dictionary -> Dictionary -> Maybe Text
guessWord _ [] = Nothing
guessWord _ [d] = Just d
guessWord fd gd = let bc = balancedChar gd in if null bc then Just (head gd) else go fd bc
  where
    go [] [] = Nothing
    go d [] = Just (head d)
    go d ch@(c : cs) =
      let d' = filter (hasLetter c) d
       in case d' of
            [] -> go d cs
            [w] -> Just w
            ws -> go d' cs

guessWord' :: Dictionary -> Dictionary -> Guess -> Maybe Text
guessWord' fd gd g = guessWord fd (reduceG g gd)

checkGuess :: Text -> Text -> Guess
checkGuess w g = zipWith3 f [0 .. 4] (T.unpack w) (T.unpack g)
  where
    f i l1 l2
      | l1 == l2 = Correct l1 i
      | hasLetter l2 w = Contains l2 i
      | otherwise = Incorrect l2

solve :: Dictionary -> Dictionary -> Text -> [Text]
solve fd gd w = go 20 gd
  where
    go 0 _ = []
    go n d = let Just g = guessWord fd d in if g == w then [g] else g : go (n - 1) (reduceG (checkGuess w g) d)
