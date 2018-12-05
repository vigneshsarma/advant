module Advent2017.Day04 (part1
                        , part2
                        ) where

import qualified Data.List as L

validPass :: (String -> String -> Bool) -> [String] -> Bool
validPass fn (p:ps)
  | foldl (\a i-> a && (fn p i)) True ps = validPass fn ps
  | otherwise = False
validPass _ [] = True

validateAll :: (String -> String -> Bool) -> String -> Int
validateAll fn ls = length $ filter id $ map (validPass fn) $ map words $ lines ls

part1:: String -> Int
part1 = validateAll (/=)

anagramCompare :: String -> String -> Bool
anagramCompare x y = L.sort x /= L.sort y

part2:: String -> Int
part2 = validateAll anagramCompare
