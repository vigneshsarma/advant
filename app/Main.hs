module Main where

-- import Lib

import System.Environment (getArgs)
import qualified Advent2017.Day01 as Day01
import qualified Advent2017.Day02 as Day02
import qualified Advent2017.Day03 as Day03
import qualified Advent2017.Day04 as Day04
import qualified Advent2017.Day05 as Day05
import qualified Advent2017.Day06 as Day06
import qualified Advent2017.Day07 as Day07
import qualified Advent2017.Day08 as Day08

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
               "04part1" -> show $ Day04.part1 input
               "04part2" -> show $ Day04.part2 input
               "05part1" -> show $ Day05.part1 input
               "05part2" -> show $ Day05.part2 input
               "06part1" -> show $ Day06.part1 input
               "06part2" -> show $ Day06.part2 input
               "07part1" -> show $ Day07.part1 input
               "07part2" -> show $ Day07.part2 input
               "08part1" -> show $ Day08.part1 input
               "08part2" -> show $ Day08.part2 input
               _ -> "no solution found"
