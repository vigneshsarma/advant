module Advent2017.Day07 (part1
                        , part2
                        , parseTree
                        ) where

import qualified Data.List as L
-- import qualified Data.Tree as T
import qualified Data.HashMap.Lazy as M
import qualified Text.ParserCombinators.Parsec as P
-- import qualified Text.ParserCombinators.Parsec.Token as PT

type Node = (String, Int, [String])
type Tree = M.HashMap String Node

treeNodes :: P.GenParser Char st [Node]
treeNodes = P.endBy node eol

nodeName :: P.GenParser Char st String
nodeName = P.many P.letter

number :: P.GenParser Char st Int
number = do
  n <- P.many P.digit
  return (read n :: Int)

eol :: P.GenParser Char st Char
eol = P.char '\n'

comaSeperatedLeaves :: P.GenParser Char st [String]
comaSeperatedLeaves = P.sepBy nodeName $ do
  _ <- P.char ','
  P.spaces

leaves :: P.GenParser Char st [String]
leaves = do
  _ <- P.string " ->"
  P.spaces
  ls <- comaSeperatedLeaves
  return ls

node :: P.GenParser Char st Node
node = do
  name <- nodeName
  P.spaces
  _ <- P.char '('
  weight <- number
  _ <-  P.char ')'
  l <- P.option [] leaves
  return (name, weight, l)

parseTree :: String -> Either P.ParseError [Node]
parseTree input = P.parse treeNodes "error" input

lookupOrThrowUp :: M.HashMap String b -> String -> b
lookupOrThrowUp m k= case M.lookup k m of
                        Just b -> b
                        Nothing -> error $ "key '" ++ k ++ "' missing."

findRoot :: Tree -> Tree
findRoot m = M.foldr (\(_, _, l) a -> foldr M.delete a l) m m

part1:: String -> String
part1 ls = head $ M.keys $ findRoot m
  where
    m = M.fromList $ map (\(n, w, l) -> (n, (n, w, l))) p
    p = case parseTree ls of
          Right p' -> p'
          Left e -> error $ show e

verifyWeight :: Tree -> Node -> (String, Int)
verifyWeight _ (n, w, []) = (n, w)
verifyWeight m (n, w, l) = if isCorrect then (n, w + (sum $ map snd ws)) else ("", 0)
  where
    isCorrect = if (length $ L.nubBy (\(_, a) (_, b) -> a==b) ws) > 1
                then error $ show (n, w, l) ++ show ws else True
    ws = map (verifyWeight m . lookupOrThrowUp m) l

-- will throw a error if weights are wrong
part2:: String -> (String, Int)
part2 ls = verifyWeight m t
  where
    -- not needed, kept for referance
    -- T.drawTree $ T.unfoldTree (\(n, w, ls) -> (show (n, w), map (\l -> lookupOrThrowUp l m) ls)) t
    t = head  $ M.elems $ findRoot m
    m = M.fromList $ map (\(n, w, l) -> (n, (n, w, l))) p
    p = case parseTree ls of
          Right p' -> p'
          Left e -> error $ show e
