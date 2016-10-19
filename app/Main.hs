module Main where

import           Control.Monad
import           Data.Array.IO
import           Data.List
import           Lib
import           Prelude       hiding (Word)
import           System.Random


main :: IO ()
main = solvePuzzle

solvePuzzle :: IO ()
solvePuzzle = do
  s <- solutions
  let
    cond puzzle = partialsCount puzzle == 0 && lettersToEmptyRatio puzzle >= 0.7
    eureka = find cond s
  case eureka of
    Nothing -> putStrLn "No solution :("
    Just p  -> print p


solutions :: IO [Puzzle]
solutions = do
  words <- readWords "words.nl.5.2000.txt"
  let puzzle = emptyPuzzle 7 7
  return $ solve words puzzle


printWords :: IO ()
printWords = do
  words <- readWords "words.nl.5.txt"
  mapM_ putStrLn words


readWords :: String -> IO [Word]
readWords filename = do
  content <- readFile filename
  shuffle (lines content)

-- | Randomly shuffle a list
--   /O(N)/
-- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
