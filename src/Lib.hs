{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( solve
    , emptyPuzzle
    , Word
    , Puzzle
    , partialsCount
    , lettersToEmptyRatio
    ) where

import           Control.Arrow (first)
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Prelude       hiding (Word)

type Word = String
type WordList = [Word]

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


isBlack :: Pixel -> Bool
isBlack Black = True
isBlack _     = False


isEmptyOrBlack :: Pixel -> Bool
isEmptyOrBlack pixel = isEmpty pixel || isBlack pixel


isPartial :: Pixel -> Bool
isPartial (Letter (Horz _)) = True
isPartial (Letter (Vert _)) = True
isPartial _                 = False


partialsInRow :: Row -> Int
partialsInRow row = partialsInRow' (Empty : row ++ [Empty]) where
  partialsInRow' (a:xs@(b:c:ys)) =
    partialsInRow' xs + case b of
      Letter (Vert _) ->
        if isEmptyOrBlack a && isEmptyOrBlack c then 0 else 1
      _               -> 0
  partialsInRow' _ = 0


partialsCount :: Puzzle -> Int
partialsCount puzzle =
  sum (map partialsInRow puzzle) +
    sum (map partialsInRow (flipPuzzle puzzle))


matchLetter :: Pixel -> Char -> Bool
matchLetter Black _       = False
matchLetter Empty _       = True
matchLetter (Letter l) ch = letter l == ch


extendLetter :: Letter -> Letter
extendLetter (Vert ch) = Both ch
extendLetter l         = l


nonEmpty :: Puzzle -> Bool
nonEmpty = all $ all $ not . isEmpty


countLetters :: Puzzle -> Int
countLetters = sum . fmap (foldr sumLetters 0) where
  sumLetters (Letter _) acc = acc + 1
  sumLetters Black acc      = acc + 1
  sumLetters _ acc          = acc


pixelsCount :: Puzzle -> Int
pixelsCount puzzle = length puzzle * length (head puzzle)


lettersToEmptyRatio :: Fractional a => Puzzle -> a
lettersToEmptyRatio puzzle =
  fromIntegral (countLetters puzzle) / fromIntegral (pixelsCount puzzle)


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


emptyPuzzle :: Int -> Int -> Puzzle
emptyPuzzle w h = replicate h $ emptyRow w


flipPuzzle :: Puzzle -> Puzzle
flipPuzzle = fmap (fmap flipPixel) <$> transpose where
  flipPixel (Letter l) = Letter $ flipLetter l
  flipPixel p          = p


solve :: WordList -> Puzzle -> [Puzzle]
solve [] puzzle = [puzzle]
solve words puzzle =
  -- Solve without the first word
  solve (tail words) puzzle ++
  -- Recurse on all permutations of all words in each row on each position
  -- on a flipped puzzle, with the remaining wordt list.
  concatMap (uncurry (flip solve))
    (first flipPuzzle <$> fitWordsIntoPuzzle words puzzle)


-- All possible permutations of fitting a word into a puzzle
fitWordsIntoPuzzle :: WordList -> Puzzle -> [(Puzzle, WordList)]
fitWordsIntoPuzzle words = concatMapRows (fitWordsIntoRow words)


concatMapRows :: (Row -> [(Row, WordList)]) -> Puzzle -> [(Puzzle, WordList)]
concatMapRows rowFn puzzle =
  concatMap mapRow $ init $ subLists puzzle where
    mapRow (h, row:t) = map (first (assemblePuzzle h t)) $ rowFn row
    assemblePuzzle h t row = h ++ [row] ++ t


-- All permutations of fitting each word in a row
fitWordsIntoRow :: WordList -> Row -> [(Row, WordList)]
fitWordsIntoRow words row = mapMaybe tryFit rowOffsets where
  -- Split row into all possible offets
  rowOffsets = subLists row
  -- Fits are only viable if either the last of head is [], Empty or Black
  -- When Empty it is replaced by Black
  tryFit :: (Row, Row) -> Maybe (Row, WordList)
  tryFit ([], t) = searchWord words t
  tryFit (h, t) = case last h of
    Empty -> tryFitAppend (init h ++ [Black]) t
    Black -> tryFitAppend h t
    _     -> Nothing
  -- If the tail fits, then append the head
  tryFitAppend h t = first (h ++) <$> searchWord words t


-- Search for a word that can fit in the row
-- When found return the updated Row and the remaining WordList
searchWord :: WordList -> Row -> Maybe (Row, WordList)
searchWord = searchWordAcc [] where
  searchWordAcc _ [] _ = Nothing
  searchWordAcc acc (word:rest) row =
    case fitWord word row of
      Nothing  -> searchWord rest row
      Just row -> Just (row, acc ++ rest)


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
  subListsAcc acc []        = [(acc, [])]
  subListsAcc acc xs@(y:ys) = (acc, xs) : subListsAcc (acc ++ [y]) ys
