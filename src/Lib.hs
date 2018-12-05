module Lib
    ( splitBy,
      alterWithDefault
    ) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as H (lookupDefault, insert, HashMap)

-- https://stackoverflow.com/a/4981265/827024
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (c==) s of
                      "" -> []
                      s' -> w : splitBy c s''
                        where (w, s'') = break (c==) s'

alterWithDefault :: (Eq k, Hashable k)
  => (v -> v -> v)
  -> v
  -> v
  -> k
  -> H.HashMap k v
  -> H.HashMap k v
alterWithDefault f dv v k m = H.insert k (f (H.lookupDefault dv k m) v) m


someFunc :: IO ()
someFunc = putStrLn "someFunc"
