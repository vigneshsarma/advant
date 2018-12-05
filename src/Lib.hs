module Lib
    ( splitBy
    ) where

-- https://stackoverflow.com/a/4981265/827024
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (c==) s of
                      "" -> []
                      s' -> w : splitBy c s''
                        where (w, s'') = break (c==) s'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
