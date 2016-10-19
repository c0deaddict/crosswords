{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( solve
    ) where

import           Control.Arrow (first)
import           Data.List
import           Data.Maybe
import           Prelude       hiding (Word)

type Word = String
type WordList = [Word]
type Shape = (Int, Int)

data Letter
  = Horz Char
  | Vert Char
  | Both Char

data Pixel
  = Letter Letter
  | Black
  | Empty

type Row = [Pixel]
type Puzzle = [Row]

instance Show Pixel where
  show (Letter (Horz ch)) = ch : "-"
  show (Letter (Vert ch)) = ch : "|"
  show (Letter (Both ch)) = ch : " "
  show Black              = "##"
  show Empty              = "??"

instance {-# OVERLAPPING #-} Show Puzzle where
  show p = intercalate "\n" (map row p) where
    row = concatMap (pad . show)
    pad x = " " ++ x ++ " "


isEmpty :: Pixel -> Bool
isEmpty Empty = True
isEmpty _     = False


isPartial :: Pixel -> Bool
isPartial (Letter (Both _)) = False
isPartial Black             = False
isPartial _                 = True


matchLetter :: Pixel -> Char -> Bool
matchLetter Black _       = False
matchLetter Empty _       = True
matchLetter (Letter l) ch = letter l == ch


extendLetter :: Letter -> Letter
extendLetter (Vert ch) = Both ch
extendLetter l         = l


completed :: Puzzle -> Bool
completed = all $ all $ not . isEmpty


letter :: Letter -> Char
letter (Vert ch) = ch
letter (Horz ch) = ch
letter (Both ch) = ch

flipLetter :: Letter -> Letter
flipLetter (Vert ch) = Horz ch
flipLetter (Horz ch) = Vert ch
flipLetter other     = other


emptyRow :: Int -> Row
emptyRow w = replicate w Empty


emptyPuzzle :: Shape -> Puzzle
emptyPuzzle (w, h) = replicate h $ emptyRow w


flipPuzzle :: Puzzle -> Puzzle
flipPuzzle = fmap (fmap flipPixel) <$> transpose where
  flipPixel (Letter l) = Letter $ flipLetter l
  flipPixel p          = p


filterWords :: Shape -> WordList -> WordList
filterWords (w, h) = filter (\word -> length word <= max w h)


solve :: WordList -> Shape -> Puzzle
solve words shape = undefined
  -- try every word on position 1
  --   if it fits.
  --   make a black after it
  --   recurse with:
  --     words = words with word removed
  --     solution with word filled in
  -- if no more word fits: then stop


-- given the current puzzle, try to fill in a word from the list
iter :: WordList -> Puzzle -> Puzzle
iter [] puzzle               = puzzle
iter _ []                    = []
iter (word:words) (row:rows) = undefined
  -- test all possible offsets: [1..(length row - length word)]
  -- for each offset:
  --   if word fits:
  --     if there is a next cell after word:
  --       it should be black or Nothing
  --       if nothing then set it to Black
  --     recurse on iter

  -- offsets = [1..(length row - length word)]
  -- fitWord word row
  -- for every word in words
    -- for y in 1..h
      -- for x in 1..w
        -- check if word fits horizontal
          -- if it does: fill it in and recurse
        -- -- check if word fits vertical (maybe via flipping)

-- All possible permutations of fitting a word in a row
fitWordPermutations :: Word -> Row -> [Row]
fitWordPermutations word row = mapMaybe tryFit rowOffsets where
  minLength r = length r >= length word
  -- Split row into all possible offets. Tail must have min length of word
  rowOffsets = filter (minLength . snd) $ subLists row
  -- If the tail fits, then append the head
  tryFit (h, t) = (++) h <$> fitWord word t


-- Try to fit a word in a row. Fills the word into the row if it fits.
-- If the word doesn't end in the last cell of the row, a Black cell is required
-- (and filled in if it was Empty) after the word.
fitWord :: Word -> Row -> Maybe Row
fitWord [] []            = Just []
fitWord _  []            = Nothing
fitWord [] row@(Black:_) = Just row
fitWord [] (Empty:rest)  = Just (Black:rest)
fitWord [] _             = Nothing
fitWord (ch:word) (pixel:row) =
  if matchLetter pixel ch then
    let
      pixel' = case pixel of
        Empty    -> Letter $ Horz ch
        Letter l -> Letter $ extendLetter l
    in (:) pixel' <$> fitWord word row
  else
    Nothing


-- Split a list on every possible index
-- subLists [1,2,3] =
--   [ ([], [1,2,3])
--   , ([1], [2,3])
--   , ([1,2], [3])
--   , ([1,2,3], [])
--   ]
subLists :: [a] -> [([a], [a])]
subLists = subListsAcc [] where
  subListsAcc :: [a] -> [a] -> [([a], [a])]
  subListsAcc acc []     = []
  subListsAcc acc (x:xs) = (acc, xs) : subListsAcc (acc ++ [x]) xs
