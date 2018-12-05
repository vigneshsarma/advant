module Advent2018.Day01 where
import Data.Char
import Data.HashSet (empty, insert, member, HashSet)

-- https://stackoverflow.com/a/4981265/827024
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (c==) s of
                      "" -> []
                      s' -> w : splitBy c s''
                        where (w, s'') = break (c==) s'


readInput :: String -> [Int]
readInput ls = map (\x -> read (dropWhile ('+'==) $ (dropWhile isSpace) x)::Int) (splitBy '\n' ls)

part2 :: String -> Int
part2 ls = reduce (0, empty) $ cycle ls'
  where
    -- reduce :: (Int, HashSet Int) -> [Int] -> Int
    reduce (a, s) (x:xs)
      | a `member` s = a
      | otherwise = reduce (x + a, insert a s) xs

    ls' = readInput ls

part1 :: String -> Int
part1 ls = foldr (+) 0 ls'
  where
    ls' = readInput ls
