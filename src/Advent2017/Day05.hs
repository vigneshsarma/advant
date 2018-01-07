module Advent2017.Day05 (part1
                        , part2
                        ) where

import Data.Array

-- not required since moving to array instead of list
-- update :: Int -> a -> [a] -> [a]
-- update i l ls = update' $ zip [0..] ls
--   where
--     update' [] = []
--     update' ((i', l'):ls')
--       | i' == i = l:(update' ls')
--       | otherwise = l':(update' ls')

jumpOut :: Int -> Int -> (Int -> Int) -> Array Int Int -> Int
jumpOut i s inc ls
  | i >= length ls || i < 0 = s
  | otherwise = jumpOut (i + x) (s+1) inc (ls // [(i, (inc x))])
  where
    x = ls ! i

inToArray :: String -> Array Int Int
inToArray ls = listArray (0, length ls' - 1) ls'
  where
    ls' = map (\i -> read i :: Int ) $ words ls

part1:: String -> Int
part1 ls = jumpOut 0 0 (1+) $ inToArray ls

part2:: String -> Int
part2 ls = jumpOut 0 0 (\i-> if i >= 3 then i-1 else i+1) $ inToArray ls
