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
import qualified Advent2017.Day09 as Day09
import qualified Advent2017.Day10 as Day10
import qualified Advent2018.Day01 as Day1801
import qualified Advent2018.Day02 as Day1802
import qualified Advent2018.Day03 as Day1803

fInput = ""

main :: IO ()
main = do
  args <- getArgs
  -- print args
  [problem, input] <- getArgs
  -- fInput <- readFile input
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
               "09part1" -> show $ Day09.part1 fInput
               "09part2" -> show $ Day09.part2 fInput
               "10part1" -> show $ Day10.part1 input
               "1801p1" -> show $ Day1801.part1 input
               "1801p2" -> show $ Day1801.part2 input
               "1802p1" -> show $ Day1802.part1 input
               "1802p2" -> show $ Day1802.part2 input
               "1803p1" -> show $ Day1803.part1 input
               "1803p2" -> show $ Day1803.part2 input
               _ -> "no solution found"
