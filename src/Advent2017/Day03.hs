module Advent2017.Day03 (part1
                        , part2
                        ) where

import qualified Data.HashMap.Lazy as M
import Data.HashMap.Lazy (HashMap)

findRowAndStart :: Int -> [Int] -> Int -> (Int, Int)
findRowAndStart l (0:xs) _ = findRowAndStart l xs 1
findRowAndStart l (x:xs) e
  | (x * 8 + e) >= l = (x, e)
  | otherwise = findRowAndStart l xs (x*8+e)

findManhattanDistance :: Int -> Int
findManhattanDistance 1 = 0
findManhattanDistance n = r + (minimum $ map (abs . (n-)) $ map (\i->b+(r*i)) [1,3..8])
  where
    (r, b) = findRowAndStart n [0..] 0

part1:: String -> Int
part1 ls = findManhattanDistance $ read ls :: Int

addPoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoint (x, y) (a, b) = (x + a, y + b)

spiral1 :: [(Int, Int)]
spiral1 = (0, 0):spiral2 (cycle [(0, 1), (-1, 0), (0, -1), (1, 0)]) (0, 0) 0

surroundingCells :: [(Int, Int)]
surroundingCells = take 8 $ drop 1 $ spiral1

spiral2 :: [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
spiral2 (c:cs) (x, y) r
  | (x,y) == (r, -r) = ((x+1), y):spiral2 (c:cs) ((x+1), y) (r+1)
  | otherwise = (nx,ny):spiral2 cs' (nx,ny) r
    where
      cs' = if (abs nx) == (abs ny) then cs else c:cs
      (nx,ny) = addPoint c (x, y)

sumAd :: HashMap (Int, Int) Int -> [(Int, Int)] -> Int -> Int
sumAd m ((0, 0):xs) c = sumAd (M.insert (0, 0) 1 m) xs c
sumAd m (x:xs) c = if xSum > c then xSum
                   else sumAd (M.insert x xSum m) xs c
  where
    xSum = sum $ fmap sum $ map (\x' -> M.lookup (addPoint x x') m) surroundingCells

part2:: String -> Int
part2 ls = sumAd M.empty spiral1 (read ls :: Int)
