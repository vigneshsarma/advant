import qualified Advent2017.Day09 as Day09

import Test.Hspec
import Control.Exception (assert)


main :: IO ()
main = do
  print $ assert (1 == Day09.part1 "{}" ) '.'
  print $ assert (3 == Day09.part1 "{{{}}}" ) '.'
  print $ assert (3 == Day09.part1 "{{},{}}" ) '.'
  print $ assert (6 == Day09.part1 "{{{}, {}, {{}}}}" ) '.'
  print $ assert (1 == Day09.part1 "{<{},{},{{}}>}" ) '.'
  print $ assert (1 == Day09.part1 "{<a>,<a>,<a>}" ) '.'
  print $ assert (5 == Day09.part1 "{{<a>},{<a>},{<a>},{<a>}}" ) '.'
  print $ assert (2 == Day09.part1 "{{<!>},{<!>},{<!>},{<a>}}" ) '.'
  print $ assert (9 == Day09.part1 "{{<ab>},{<ab>},{<ab>},{<ab>}}" ) '.'
