module Main where

import Data.List (group, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Wordle.Solver

main =
  do
    fd <- T.lines <$> TIO.readFile "frontend/wordle-allowed-guesses.txt"
    gd <- T.lines <$> TIO.readFile "frontend/wordle-answers-alphabetical.txt"
    let scores = map (length . solve (fd ++ gd) gd) gd
    let hist = let g = (group . sort) scores in zip (map head g) (map length g)

    --putStrLn $ "maximum solve: " ++ show (maximum scores)
    --putStrLn $ "average solve: " ++ show (fromIntegral (sum scores) / fromIntegral (length scores))
    --putStrLn $ "hist solve:" ++ show hist
    print $ solve fd gd (T.pack "scale")
