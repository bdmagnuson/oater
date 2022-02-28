module Main where

import Data.List (group, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Wordle.Solver

main =
  do
    fd <- T.lines <$> TIO.readFile "wordle-allowed-guesses.txt"
    gd <- T.lines <$> TIO.readFile "wordle-answers-alphabetical.txt"
    let scores = map (length . solve fd gd) gd
    let hard = map fst (filter (\(_, s) -> s == 6) (zip gd scores))
    let hist = let g = (group . sort) scores in zip (map head g) (map length g)

    putStrLn $ "maximum solve: " ++ show (maximum scores)
    putStrLn $ "average solve: " ++ show (fromIntegral (sum scores) / fromIntegral (length scores))
    putStrLn $ "hist solve:" ++ show hist
    putStrLn $ (show hard)
