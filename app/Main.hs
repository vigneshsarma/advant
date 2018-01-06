module Main where

import Lib

import System.Environment (getArgs)
import qualified Advent2017.Day01 as Day01
import qualified Advent2017.Day02 as Day02
import qualified Advent2017.Day03 as Day03

main :: IO ()
main = do
  [problem, input] <- getArgs
  putStrLn $ case problem of
               "01part1" -> show $ Day01.part1 input
               "01part2" -> show $ Day01.part2 input
               "02part1" -> show $ Day02.part1 input
               "02part2" -> show $ Day02.part2 input
               "03part1" -> show $ Day03.part1 input
               "03part2" -> show $ Day03.part2 input
               _ -> "no solution found"
