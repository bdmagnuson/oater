module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Wordle.Solver

main =
  do
    -- d <- T.lines <$> TIO.readFile "wordlelist.txt"
    d <- T.lines <$> TIO.readFile "wordlist.txt"
    let scores = map (length . solve d) (take 200000 d)
    print $ filter (\(_, s) -> s == 8) (zip d scores)

    putStrLn $ "maximum solve: " ++ show (maximum scores)
    putStrLn $ "average solve: " ++ show (fromIntegral (sum scores) / fromIntegral (length scores))
