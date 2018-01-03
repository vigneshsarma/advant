module Advent2017.Day02 (part1
                        , part2
                        )
where

import qualified Data.List as L

diffMaxAndMin :: [Int] -> Int
diffMaxAndMin xs = (last xs) - (head xs)

sumDivided :: [Int] -> [Int]
sumDivided [] = []
sumDivided (x:xs) = (map (\ i -> if (mod i x) == 0 then div i x else 0) xs) ++ sumDivided xs

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum fn m = sum $ map fn m

toSortedIntMatrix :: String -> [[Int]]
toSortedIntMatrix ls = map (L.sort . map (\i -> read i :: Int)) chMatrix
  where chMatrix = map words  $ lines ls

part1:: String -> Int
part1 ls = checksum diffMaxAndMin $ toSortedIntMatrix ls

part2:: String -> Int
part2 ls = checksum (sum . sumDivided) $ toSortedIntMatrix ls
