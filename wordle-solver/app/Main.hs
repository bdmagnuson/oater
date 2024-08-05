module Main where

import Data.List (group, sort)
import Data.Text (Text)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Wordle.Solver

main =
  do
    fd <- T.lines <$> TIO.readFile "frontend/wordle-allowed-guesses.txt"
    gd <- T.lines <$> TIO.readFile "frontend/wordle-answers-alphabetical.txt"

    let gds = S.fromList gd
    let fds = S.union gds (S.fromList fd)

    let scores = map (length . solve fds gds) gd
    let hist = let g = (group . sort) scores in zip (map head g) (map length g)

    putStrLn $ "maximum solve: " ++ show (maximum scores)
    putStrLn $ "average solve: " ++ show (fromIntegral (sum scores) / fromIntegral (length scores))
    putStrLn $ "hist solve:" ++ show hist
    --print $ solve fds gds (T.pack "snowy")
