{-# LANGUAGE PartialTypeSignatures #-}
module Advent2017.Day09 (part1, part2, parse) where

type ParseState = (Int, Bool, Bool)

parse :: String -> ParseState -> (Int, Int)
parse [] _ = (0, 0)
parse (_:ls) (d, c, True) = parse ls (d, c, False)
parse ('!':ls) (d, c, False) = parse ls (d, c, True)
parse ('<':ls) (d, False, False) = parse ls (d, True, False)
parse ('>':ls) (d, True, False) = parse ls (d, False, False)
parse (_:ls) (d, True, False) = (g, 1+c)
  where (g, c) = parse ls (d, True, False)
parse ('{':ls) (d, False, False) = parse ls (d+1, False, False)
parse ('}':ls) (d, False, False) = (d + g, c)
  where (g, c) = parse ls (d-1, False, False)
parse (_:ls) (d, False, False) = parse ls (d, False, False)

part1 :: String -> Int
part1 ls = g
  where (g, _) = parse ls (0, False, False)

part2 :: String -> Int
part2 ls = c
  where (_, c) = parse ls (0, False, False)
