{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import Data.FileEmbed
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Reflex.Dom
import Wordle.Solver

main :: IO ()
main = mainWidgetWithCss css bodyElement
  where
    css = $(embedFile "css/simple.css")

indexA y x = fmap ((!! x) . (!! y))

bisequence :: MonadWidget t m => m [(Dynamic t Color, Event t ())] -> m (Dynamic t [Color], Event t ())
bisequence = fmap ((\(a, b) -> (sequence a, mergeWith (const id) b)) . unzip)

bi2 :: MonadWidget t m => m [(Dynamic t [Color], Event t ())] -> m (Dynamic t [[Color]], [Event t ()])
bi2 = fmap ((\(a, b) -> (sequence a, b)) . unzip)

bodyElement :: MonadWidget t m => m ()
bodyElement = mdo
  el "h1" $ text "Wordle Solver"
  elClass "div" "container" $ mdo
    let guess = makeGuess <$> colors
    let clear = map (\n -> mconcat (evReset : (take n clicks))) [0 .. 5]
    (colors, clicks) <- bi2 $ forM [0 .. 5] (\y -> bisequence $ forM [0 .. 4] (\x -> box (clear !! y) (indexA y x $ guess)))
    return ()
  evReset <- elClass "div" "button" (button "Reset")
  return ()

box :: MonadWidget t m => Event t b -> Dynamic t T.Text -> m (Dynamic t Color, Event t ())
box r i = mdo
  (e, _) <- elDynAttr' "div" (attrs <$> dClass) (dynText i)
  let ev = domEvent Click e
  dClass <- foldDyn ($) Black $ leftmost [nextColor <$ ev, const Black <$ r]
  return (dClass, ev)

-- attrs :: Color -> T.Text
attrs Black = "data-state" =: "empty" <> "class" =: "tile" <> "data-animation" =: "pop1"
attrs Grey = "data-state" =: "absent" <> "class" =: "tile" <> "data-animation" =: "pop2"
attrs Yellow = "data-state" =: "present" <> "class" =: "tile" <> "data-animation" =: "pop3"
attrs Green = "data-state" =: "correct" <> "class" =: "tile" <> "data-animation" =: "pop4"

Just g0 = Just "salet" --guessWord' fd gd []

makeGuess :: [[Color]] -> [[T.Text]]
makeGuess words = map (map T.singleton . T.unpack) gs
  where
    gs = g0 : map ff [1 .. 5]
    ff x =
      if all hasGuess (take x words)
        then case guessWord' (fd ++ gd) gd $ concat (zipWith g (take x gs) (take x words)) of
          Just x -> x
          Nothing -> "XXXXX"
        else "     "

    g :: T.Text -> [Color] -> Guess
    g a b = zipWith h (zip [0 ..] (T.unpack a)) b
    h :: (Int, Char) -> Color -> Letter
    h _ Black = NoAnswer
    h (_, c) Grey = Incorrect c
    h (i, c) Yellow = Contains c i
    h (i, c) Green = Correct c i

hasGuess :: [Color] -> Bool
hasGuess x = Black `notElem` x && not (all (== Green) x)

data Color = Black | Grey | Yellow | Green deriving (Eq)

nextColor Grey = Yellow
nextColor Yellow = Green
nextColor Green = Black
nextColor Black = Grey

gdbs = $(embedFile "wordle-answers-alphabetical.txt")

fdbs = $(embedFile "wordle-allowed-guesses.txt")

fd = (T.lines . T.pack . BS.unpack) fdbs

gd = (T.lines . T.pack . BS.unpack) gdbs
