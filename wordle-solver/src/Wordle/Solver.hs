{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Wordle.Solver (guessWord', solve, Letter (..), Guess, foo2, lookupW') where

import Control.Lens
import Data.Char
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List
import Data.Map.Lazy qualified as M
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set qualified as S
import Data.Text.IO qualified as TIO
import Data.Hashable
import Data.Function.Memoize
import GHC.IO.Unsafe
import GHC.Generics (Generic)
import Debug.Trace

data Letter
  = Correct Char Int
  | Contains Char Int
  | Incorrect Char
  | NoAnswer
  deriving (Show, Generic, Eq, Ord)

instance Hashable Letter

data Tree = Node Tree Tree Tree | Leaf Int deriving (Show)

makeBaseFunctor ''Tree

data Div = NodeD (M.Map Letter Div) | LeafD Int


construct :: Dictionary -> Div
construct = go 0
  where go 5 d = LeafD (length d)
        go _ [] = LeafD 0
        go n d = let map = M.fromList [(k, go (n + 1) (reduceL k d)) | k <- ks n] in NodeD map

ks = memoize f
  where f n = concatMap (\k -> [Incorrect k, Contains k n, Correct k n]) ['a'..'z']

lookupW :: Dictionary -> Text -> [Int]
lookupW gd w = map (lookupG . reverse) gs
  where gs = gooo 0 (T.unpack w) []
        div = construct gd
        lookupG :: Guess -> Int 
        lookupG g = go g div
          where go _ (LeafD n) = n
                go [] (NodeD _) = error "got to end?"
                go (g:gs) (NodeD d) = go gs (d ^?! ix g)


lookupW' :: Dictionary -> Dictionary
lookupW' gd = sortOn (length . filter (/=0) . map (lookupG . reverse) . gs) gd
  where gs w = gooo 0 (T.unpack w) []
        div = construct gd
        lookupG :: Guess -> Int 
        lookupG g = go g div
          where go _ (LeafD n) = n
                go [] (NodeD _) = error "got to end?"
                go (g:gs) (NodeD d) = go gs (d ^?! ix g)

gooo :: Int -> String -> [Guess] -> [Guess]
gooo _ "" gs = gs
gooo n (c:cs) [] = gooo (n + 1) cs [[Incorrect c], [Correct c n], [Contains c n]]
gooo n (c:cs) gs = gooo (n + 1) cs (map (Incorrect c:) gs ++ map (Correct c n:) gs ++ map (Contains c n:) gs)

foo3 :: Dictionary -> Text -> Int
foo3 d t = hylo gather distribute (d, T.unpack t, 0)
  where
    distribute ([], _, _) = LeafF 0
    distribute (d, "", _) = LeafF 1
    distribute (d, c : cs, p) = NodeF (correct, cs, p + 1) (contains, cs, p + 1) (incorrect, cs, p + 1)
      where
        (correct, contains, incorrect) = reduceL' d (c, p)
    gather (LeafF n) = n
    gather (NodeF a b c) = a + b + c

reduceL' :: Dictionary -> (Char, Int) -> (Dictionary, Dictionary, Dictionary)
reduceL' d (c, i) = foldl go ([], [], []) d
  where
    go (corr, cont, inc) w
      | T.index w i == c = let a = (w:corr, cont, inc) in a
      | T.any (== c) w = let b = (corr, w:cont, inc) in b
      | otherwise = let c = (corr, cont, w:inc) in c

foo2 :: Dictionary -> Dictionary -> Dictionary
foo2 fd gd = sortOn (foo3 gd) fd

stddev' :: [Float] -> Float
stddev' xs = sqrt . average . map ((^2) . (-) axs) $ xs
           where average = (/) <$> sum <*> realToFrac . length
                 axs     = average xs

stddev = stddev' . map fromIntegral

type Guess = [Letter]

type Dictionary = [Text]

applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

hasLetter :: Char -> Text -> Bool
hasLetter c t = isJust (T.find (== c) t)

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
guessWord fd [] = Nothing
guessWord fd [gd] = Just gd
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
--solve fd gd w = "trace" : go 20 (reduceG (checkGuess w "trace") gd)
solve fd gd w = go 20 gd
  where
    go 0 _ = []
    go _ [d] = [d]
    go n d = let Just g = guessWord fd d in if g == w then [g] else g : go (n - 1) (reduceG (checkGuess w g) d)

gd = unsafePerformIO (T.lines <$> TIO.readFile "../frontend/wordle-answers-alphabetical.txt")

fd = unsafePerformIO (T.lines <$> TIO.readFile "../frontend/wordle-allowed-guesses.txt")
