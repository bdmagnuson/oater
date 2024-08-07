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
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set qualified as S
import Data.Text.IO qualified as TIO
import GHC.IO.Unsafe
import Control.Arrow

data Letter
  = Correct Char Int
  | Contains Char Int
  | Incorrect Char
  | NoAnswer
  deriving (Show, Eq)

data Tree = Node Tree Tree Tree | Leaf Int deriving (Show)

makeBaseFunctor ''Tree

type Guess = [Letter]

type Dictionary = [Text]

scoreWord :: Dictionary -> Text -> Int
scoreWord d t = hylo gather distribute (d, T.unpack t, 0)
  where
    distribute ([], _, _) = LeafF 0
    distribute (_, "", p) = LeafF 1
    distribute (d, (c:cs), p) =
         let (correct, contains, incorrect) = reduceL' d (c, p)
         in NodeF (correct, cs, p + 1) (contains, cs, p + 1) (incorrect, cs, p + 1)
    gather (LeafF n) = n
    gather (NodeF a b c) = a + b + c

applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

reduceL' :: Dictionary -> (Char, Int) -> (Dictionary, Dictionary, Dictionary)
reduceL' d (c, i) = foldl go ([], [], []) d
  where
    go (corr, cont, inc) w
      | T.index w i == c = let a = (w:corr, cont, inc) in a
      | T.any (== c) w = let b = (corr, w:cont, inc) in b
      | otherwise = let c = (corr, cont, w:inc) in c

reduceL :: Letter -> Dictionary -> Dictionary
reduceL l d =
  case l of
    Correct c i -> let (corr, _, _) = reduceL' d (c, i) in corr
    Contains c i -> let (_, cont, _) = reduceL' d (c, i) in cont
    Incorrect c -> let (_, _, inc) = reduceL' d (c, 0) in inc
    NoAnswer -> d

reduceG :: Guess -> Dictionary -> Dictionary
reduceG g = applyAll (map reduceL g)

guessWord :: Dictionary -> Dictionary -> Maybe Text
guessWord fd gd =
  case length gd of
    0 -> Nothing
    1 -> let [w] = gd in Just w
    otherwise ->
      let a = map (id &&& scoreWord gd) fd
          b = sortBy f a
          f (w1, s1) (w2, s2) =
            case compare s1 s2 of
              LT -> LT
              GT -> GT
              EQ -> if w1 `elem` gd then GT else LT
      in Just $ (fst . last) b

guessWord' :: Dictionary -> Dictionary -> Guess -> Maybe Text
guessWord' fd gd g = guessWord fd (reduceG g gd)

checkGuess :: Text -> Text -> Guess
checkGuess w g = zipWith3 f [0 .. 4] (T.unpack w) (T.unpack g)
  where
    f i l1 l2
      | l1 == l2 = Correct l1 i
      | T.any (== l2) w = Contains l2 i
      | otherwise = Incorrect l2

-- 'pre-guess' salet since that's what it's going to do anyway
solve :: Dictionary -> Dictionary -> Text -> [Text]
solve fd gd w = "salet" : go 20 (reduceG (checkGuess w "salet") gd)
  where
    go :: Int -> Dictionary -> [Text]
    go n d 
      | n == 0 = []
      | length d == 1 = d
      | otherwise = let Just g = guessWord fd d in if g == w then [g] else g : go (n - 1) (reduceG (checkGuess w g) d)

gd = unsafePerformIO (T.lines <$> TIO.readFile "../frontend/wordle-answers-alphabetical.txt")

fd = gd ++ unsafePerformIO (T.lines <$> TIO.readFile "../frontend/wordle-allowed-guesses.txt")
