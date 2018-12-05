module Advent2017.Day06 (part1
                        , part2
                        ) where

import qualified Data.HashMap.Lazy as M

maxWithIndex :: [(Int, Int)] -> (Int, Int)
maxWithIndex (l:ls) = foldl (\(ia, va) (i, v) ->
                               if v > va then (i, v)
                               else (ia, va)) l ls

reallocate :: [Int] -> [Int]
reallocate xs = map (\(j, a) -> if j == i then leftover
                      else if d /= 0 then a + inc
                      else if wnToInc j then a+inc
                      else a) ls
  where
    ls = zip [0..] xs
    (i, v) = maxWithIndex ls
    ln = (length xs)
    (d, m) = divMod v (ln -1)
    inc = if d > 0 then d else 1
    leftover =  if d == 0 then 0 else  m
    wnToInc j = if m >= (ln - i) then j > i || j <= m - (ln-i) else j> i && j <= i + m


stabilize :: Int -> M.HashMap [Int] Int -> [[Int]] -> [Int] -> (Int, Maybe Int)
stabilize c m trail ls
  | M.member ls m = (c, M.lookup ls m
                    -- , take 30 (ls:trail)
                    )
  | otherwise = stabilize (c+1) (M.insert ls c m) (ls:trail) ls'
    where
      ls' = reallocate ls

part1:: String -> Int
part1 ls = c
  where
    (c, Just _) = stabilize 0 M.empty [] ls'
    ls' = map (\i -> read i :: Int) $ words ls

part2:: String -> Int
part2 ls = c - c'
  where
    (c, Just c') = stabilize 0 M.empty [] ls'
    ls' = map (\i -> read i :: Int) $ words ls
