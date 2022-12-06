#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , containers
-}

import System.IO (readFile)
import qualified Data.Set as Set
import qualified Data.List as List

unique :: (Ord a, Eq a) => [a] -> Bool
unique as = length as == (Set.size $ Set.fromList as)

sublists :: [a] -> Int -> [[a]]
sublists a len =
  [take len $ drop x a | x <- [0..length a - len]]

marker :: String -> Int -> Maybe Int
marker s len =
  (+len) <$> List.findIndex unique (sublists s len) 

main :: IO ()
main = do
  input <- readFile "day06.txt"

  print $ "Problem 1: " <> (show $ marker input 4)
  print $ "Problem 2: " <> (show $ marker input 14)
