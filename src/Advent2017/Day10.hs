module Advent2017.Day10 where
import Lib (splitBy)

hashRound :: [Int] -> Int -> [Int]
hashRound ls n = concat [(reverse t), r]
  where
    (t, r) = splitAt n ls

knotHash :: [Int] -> [Int] -> [Int]
knotHash [] ls = ls
knotHash (x:xs) ls = knotHash xs $ hashRound ls x

part1 :: String -> Int
part1 ls = a*b
  where
    a:b:_ = knotHash (map (\x -> read x::Int) (splitBy ',' ls)) [0..5]

part2 :: String -> Int
part2 ls = undefined
