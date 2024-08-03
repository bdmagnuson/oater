{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wordle.Solver (guessWord', solve, Letter (..), Guess) where

import Control.Lens
import Data.Char
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set qualified as S
import Data.Text.IO qualified as TIO
import GHC.IO.Unsafe

data Letter
  = Correct Char Int
  | Contains Char Int
  | Incorrect Char
  | NoAnswer
  deriving (Show)

data Tree = Node Tree Tree Tree | Leaf Int deriving (Show)

makeBaseFunctor ''Tree

foo3 :: Dictionary -> Text -> [Int]
foo3 d t = hylo gather distribute (d, T.unpack t, 0)
  where
    distribute ([], _, _) = LeafF 0
    distribute (d, "", _) = LeafF (length d)
    distribute (d, c : cs, p) = NodeF (correct, cs, p + 1) (contains, cs, p + 1) (incorrect, cs, p + 1)
      where
        (correct, contains, incorrect) = reduceL' d (c, p)
    gather (LeafF n) = [n]
    gather (NodeF a b c) = a ++ b ++ c

reduceL' :: Dictionary -> (Char, Int) -> (Dictionary, Dictionary, Dictionary)
reduceL' d (c, i) = go d ([], [], [])
  where
    go [] ds = ds
    go (w : ws) (corr, cont, inc)
      | T.index w i == c = go ws (w:corr, cont, inc)
      | T.any (== c) w = go ws (corr, w:cont, inc)
      | otherwise = go ws (corr, cont, w:inc)

foo2 :: Dictionary -> Dictionary -> Dictionary
foo2 fd gd = sortOn (length . filter (/= 0) . foo3 gd) fd

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

guessWord :: Dictionary -> Dictionary -> Maybe Text
guessWord fd gd =
  case foo2 fd gd of
    [] -> Nothing
    xs -> Just (last xs)

guessWord' :: Dictionary -> Dictionary -> Guess -> Maybe Text
guessWord' fd gd g = guessWord fd (reduceG g gd)

checkGuess :: Text -> Text -> Guess
checkGuess w g = zipWith3 f [0 .. 4] (T.unpack w) (T.unpack g)
  where
    f i l1 l2
      | l1 == l2 = Correct l1 i
      | T.any (== l2) w = Contains l2 i
      | otherwise = Incorrect l2

solve :: Dictionary -> Dictionary -> Text -> [Text]
solve fd gd w = go 20 gd
  where
    go 0 _ = []
    go _ [d] = [d]
    go n d = let Just g = guessWord fd d in if g == w then [g] else g : go (n - 1) (reduceG (checkGuess w g) d)

gd = unsafePerformIO (T.lines <$> TIO.readFile "wordle-answers-alphabetical.txt")

fd = unsafePerformIO (T.lines <$> TIO.readFile "wordle-allowed-guesses.txt")
