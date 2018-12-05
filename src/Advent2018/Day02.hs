module Advent2018.Day02 where
import Lib (alterWithDefault)
import qualified Data.HashMap.Lazy as H (empty, lookupDefault, insert, HashMap, foldr)

part2 :: String -> [(String, [(String, Int)])]
part2 txt = filter (\(_, cs) -> (not . null) cs) $ diffAll $ lines txt
  where
    diff :: String -> String -> (String, Int)
    diff l m = (m, (foldr (\x a -> case x of
                                     False -> a + 1
                                     _ -> a) 0 . zipWith (==) l) m)

    diffAll :: [String] -> [(String, [(String, Int)])]
    diffAll (l:[]) = [(l, [])]
    diffAll (l:ls) = (l,  filter (\(_, i) -> i == 1) $ map (diff l) ls):(diffAll ls)



part1 :: String -> Int
part1 ls = twos * threes
  where
    (twos, threes) = addUp mapOver
    addUp = foldr (\(x, y) (a, b) -> (x + a, y + b)) (0, 0)
    mapOver = map (checkCount . freq) $ lines ls
    checkCount = H.foldr (\v (two, three) -> if v == 2 then (1, three)
                           else if v == 3 then (two, 1)
                           else (two, three)) (0, 0)
    freq = foldr (\k a -> alterWithDefault (+) 0 1 k a) H.empty
