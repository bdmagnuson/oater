{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import Data.FileEmbed
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Reflex.Dom
import Solver

main :: IO ()
main = mainWidgetWithCss css bodyElement
  where
    css = $(embedFile "css/simple.css")

indexA i = fmap (!! i)

bodyElement :: MonadWidget t m => m ()
bodyElement = mdo
  el "h1" $ text "Wordle Solver"
  elClass "div" "container" $ mdo
    let guess = makeGuess <$> colors
    colors <- sequence <$> forM [0 .. 29] (\i -> box evReset (indexA i guess))
    return ()
  evReset <- elClass "div" "button" (button "Reset")
  return ()

-- box :: MonadWidget t m => Dynamic t T.Text -> m (Dynamic t Color)
box r i = elClass "div" "whatever" $ mdo
  (e, _) <- elDynClass' "div" (attrs <$> dClass) (dynText i)
  let ev = domEvent Click e
  dClass <- foldDyn ($) Black $ leftmost [nextColor <$ ev, const Black <$ r]
  return dClass

chunksOf :: Int -> [a] -> [[a]]
chunksOf n c =
  case splitAt n c of
    (x, []) -> [x]
    (x, xs) -> x : chunksOf n xs

makeGuess :: [Color] -> [T.Text]
makeGuess cs = concatMap (map (T.singleton . toUpper) . T.unpack) [g0, g1, g2, g3, g4, g5]
  where
    words = chunksOf 5 cs
    g0 = guessWord' fd gd []
    g1 = if all hasGuess (take 1 words) then guessWord' fd gd $ concat (zipWith g [g0] (take 1 words)) else "     "
    g2 = if all hasGuess (take 2 words) then guessWord' fd gd $ concat (zipWith g [g0, g1] (take 2 words)) else "     "
    g3 = if all hasGuess (take 3 words) then guessWord' fd gd $ concat (zipWith g [g0, g1, g2] (take 3 words)) else "     "
    g4 = if all hasGuess (take 4 words) then guessWord' fd gd $ concat (zipWith g [g0, g1, g2, g3] (take 4 words)) else "     "
    g5 = if all hasGuess (take 5 words) then guessWord' fd gd $ concat (zipWith g [g0, g1, g2, g3, g4] (take 5 words)) else "     "
    g :: T.Text -> [Color] -> Guess
    g a b = zipWith h (zip [0 ..] (T.unpack a)) b
    h :: (Int, Char) -> Color -> Letter
    h _ Black = NoAnswer
    h (_, c) Grey = Incorrect c
    h (i, c) Yellow = Contains c i
    h (i, c) Green = Correct c i
    hasGuess x = Black `notElem` x && not (all (== Green) x)

data Color = Black | Grey | Yellow | Green deriving (Eq)

nextColor Grey = Yellow
nextColor Yellow = Green
nextColor Green = Black
nextColor Black = Grey

attrs :: Color -> T.Text
attrs Black = "cell-black"
attrs Grey = "cell-grey"
attrs Yellow = "cell-yellow"
attrs Green = "cell-green"

gdbs = $(embedFile "wordle-answers-alphabetical.txt")

fdbs = $(embedFile "wordle-allowed-guesses.txt")

fd = (T.lines . T.pack . BS.unpack) fdbs

gd = (T.lines . T.pack . BS.unpack) gdbs
