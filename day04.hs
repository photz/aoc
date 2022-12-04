#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , trifecta
-}

import System.IO (readFile)
import Text.Trifecta
import Data.Foldable (foldr)
import Control.Monad (mfilter)

type Range = (Integer, Integer)
type Pair = (Range, Range)

range :: Parser Range
range = do
  a <- integer
  char '-'
  b <- integer
  return (a, b)

pair :: Parser Pair
pair = do
  a <- range 
  char ','
  b <- range
  return (a, b)

pairs :: Parser [Pair]
pairs = some pair

contains :: Range -> Range -> Bool
contains (a0, a1) (b0, b1) = a0 <= b0 && b1 <= a1

overlaps :: Range -> Range -> Bool
overlaps (a0, a1) (b0, b1) = (a0 <= b0 && b0 <= a1) || (b0 <= a0 && a0 <= b1)

problem1 :: [Pair] -> Integer
problem1 pairs =
  foldr (\_ acc -> acc + 1) 0 (mfilter (\(a, b) -> contains a b || contains b a) pairs)

problem2 :: [Pair] -> Integer
problem2 pairs =
  foldr (\_ acc -> acc + 1) 0 (mfilter (\(a, b) -> overlaps a b) pairs)

main :: IO ()
main = do
  input <- readFile "day04.txt"
  let thePairs = parseString pairs mempty input
  
  print $ problem1 <$> thePairs
  print $ problem2 <$> thePairs
