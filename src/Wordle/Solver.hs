module Wordle.Solver () where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.Either
import Data.Function
import Data.List
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V

data Letter
  = Correct Char Int
  | Contains Char Int
  | Incorrect Char
  deriving (Show)

type Guess = [Letter]

type Dictionary = [Text]

hasLetter :: Char -> Text -> Bool
hasLetter c t =
  case T.find (== c) t of
    Nothing -> False
    Just _ -> True

reduceL :: Letter -> Dictionary -> Dictionary
reduceL l d =
  case l of
    Correct c i -> filter (\x -> T.index x i == c) d
    Contains c i -> (filter (\x -> T.index x i /= c) . filter (hasLetter c)) d
    Incorrect c -> filter (not . hasLetter c) d

applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

reduceG :: Guess -> Dictionary -> Dictionary
reduceG g = applyAll (map reduceL g)

guessWord :: Dictionary -> Text
guessWord d = head (go 5 d)
  where
    go 0 d = d
    go n d = go (n - 1) (filter (hasLetter (balancedChar d)) d)

balancedChar :: Dictionary -> Char
balancedChar d = (fst . head) (sortOn snd idx')
  where
    az = ['a' .. 'z']
    sub :: V.Vector (Either Int Int)
    sub = V.replicate 26 (Left (-1))
    f w v = V.zipWith (add) v (V.update sub (V.fromList [(ord c - ord 'a', Right 1) | c <- T.unpack w]))
    counts = (foldr f (V.replicate 26 (Left 0)) d) & traversed . both %~ abs
    idx = (zip az (V.toList counts)) ^.. folded . filtered (\(_, x) -> isRight x)
    idx' = (fmap . fmap) (fromRight 0) idx

add (Left a) (Left b) = Left (a + b)
add (Right a) (Left b) = Right (a + b)
add (Left a) (Right b) = Right (a + b)
add (Right a) (Right b) = Right (a + b)

checkGuess :: Text -> Text -> Guess
checkGuess w g = zipWith3 f [0 .. 4] (T.unpack w) (T.unpack g)
  where
    f i l1 l2
      | l1 == l2 = Correct l1 i
      | hasLetter l2 w = Contains l2 i
      | otherwise = Incorrect l2

solve :: Dictionary -> Text -> [Text]
solve d w = go 10 d
  where
    go 0 _ = []
    go n d = let g = guessWord d in if (g == w) then [g] else g : (go (n - 1) (reduceG (checkGuess w g) d))

main =
  do
    d <- T.lines <$> TIO.readFile "wordlist.txt"
    let scores = map (length . solve d) (take 100 d)
    putStrLn $ "maximum solve: " ++ (show (maximum scores))
    putStrLn $ "average solve: " ++ (show (fromIntegral (sum scores) / fromIntegral (length scores)))
