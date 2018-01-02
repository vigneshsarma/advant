module Advent2017.Day01 (part1, part2) where

import qualified Data.Char as C

findSameAsNext :: Int -> [Int] -> Int
findSameAsNext f (x:y:rst)
  | x == y = x+(findSameAsNext f (y:rst))
  | otherwise = findSameAsNext f (y:rst)
findSameAsNext f (x:[])
  | x == f = x
  | otherwise = 0

part1 :: [Char] -> Int
part1 ls = findSameAsNext f (f:rst)
  where
    f:rst = map C.digitToInt ls

compareAndSum :: [Int] -> [Int] -> Int
compareAndSum (x:xs) (y:ys)
  | x == y = (x*2) + (compareAndSum xs ys)
  | otherwise = compareAndSum xs ys
compareAndSum [] [] = 0

part2 :: [Char] -> Int
part2 ls = compareAndSum (take half ls') (drop half ls')
  where
    ls' = map C.digitToInt ls
    half = (length ls) `div` 2
