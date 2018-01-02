module Main where

import Lib

import System.Environment (getArgs)
import qualified Advent2017.Day01 as Day01

main :: IO ()
main = do
  [problem, input] <- getArgs
  putStrLn $ case problem of
               "01part1" -> show $ Day01.part1 input
               "01part2" -> show $ Day01.part2 input
               _ -> "no solution found"
