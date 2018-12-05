module Advent2018.Day03 where
import Prelude hiding (id)
import Lib (alterWithDefault)
import qualified Data.HashMap.Lazy as H (HashMap, empty, filter, size, foldr, elems)
import qualified Data.HashSet as S (unions, fromList, difference, HashSet)

import qualified Text.ParserCombinators.Parsec as P


data Record = Record {
  id :: Int,
  x :: Int,
  y :: Int,
  width :: Int,
  hight :: Int} deriving (Show)

-- #1 @ 432,394: 29x14
readRecord :: P.GenParser Char st Record
readRecord = do
  _ <- P.string "#"
  id <- P.many1 P.digit
  P.spaces
  _ <- P.string "@"
  P.spaces
  x <- P.many1 P.digit
  _ <- P.string ","
  y <- P.many1 P.digit
  _ <- P.string ":"
  P.spaces
  width <- P.many1 P.digit
  _ <- P.string "x"
  hight <- P.many1 P.digit
  return Record {id=read id :: Int,
                 x=read x :: Int,
                 y=read y :: Int,
                 width=read width :: Int,
                 hight=read hight :: Int}

-- H.HashMap (Int, Int) [Int] -- [(Int, [(Int, Int)])]
shadeAll :: [String] -> H.HashMap (Int, Int) [Int]
shadeAll ls = foldr (\k a -> shadeAll' k a) H.empty shadedRect
  where
    shadeAll' (id', xs) m = foldr (\k a -> alterWithDefault (++) [] [id'] k a) m xs
    shadedRect = map (\r -> (id r, cartProd [(x r) + 1..(x r) + (width r)]
                              [(y r) + 1..(y r) + (hight r)])) rs
    cartProd is js = [(i, j) | i <- is, j <- js]
    rs = map (\l -> case P.parse readRecord "error" l of
                      Right p -> p
                      Left e -> error $ show e) ls


part1 :: String -> Int
part1 txt = H.size $ H.filter (\v -> length v > 1) $ shadeAll $ lines txt

part2 :: String -> S.HashSet Int
part2 txt = S.difference allId intersRect
  where
    allId = S.fromList [1..(length ls)]
    intersRect = S.unions $ map S.fromList $ H.elems intersArea
    intersArea = H.filter (\v -> length v > 1) $ shadeAll ls
    ls = lines txt
