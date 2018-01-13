{-# LANGUAGE PartialTypeSignatures #-}
module Advent2017.Day08 (part1, part2) where

import qualified Data.HashMap.Lazy as M
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as PT

-- data Op = Inc String Int | Dec String Int deriving (Show, Eq)
-- data Cond = LThan | GThan | Eqe | GtE | LtE | NE deriving (Show, Eq)

type SubExp = (String, String, Int)
type Expr = (SubExp, SubExp)
type Regs = M.HashMap String Int

readInsts :: P.GenParser Char st Expr
readInsts = do
  reg1 <- P.many1 P.letter
  P.spaces
  op <- P.many1 P.letter
  P.spaces
  sym1 <- P.option "" $ P.string "-"
  n1 <- P.many1 P.digit
  _ <- P.string " if "
  reg2 <- P.many1 P.letter
  P.spaces
  cond <- P.manyTill P.anyChar $ P.char ' '
  P.spaces
  sym2 <- P.option "" $ P.string "-"
  n2 <- P.many1 P.digit
  return ((op, reg1, read (sym1 ++ n1) :: Int),
          (cond, reg2, read (sym2 ++ n2) :: Int))

exprs :: P.GenParser Char st [Expr]
exprs = P.endBy readInsts eol

eol :: P.GenParser Char st Char
eol = P.char '\n'

lookupOr0 :: Regs -> String -> Int
lookupOr0 m k = case M.lookup k m of
                  Just b -> b
                  Nothing -> 0

checkCondition :: SubExp -> Regs -> Bool
checkCondition (c, r, n) m = case c of
                               ">" -> lu r > n
                               "<" -> lu r < n
                               ">=" -> lu r >= n
                               "<=" -> lu r <= n
                               "==" -> lu r == n
                               "!=" -> lu r /= n
                               other -> error $ "cant handle cond '" ++ other ++ "'."
                             where
                              lu = lookupOr0 m

applyOp :: SubExp -> Regs -> (String, Int)
applyOp ("inc", r, n) m = (r, lookupOr0 m r + n)
applyOp ("dec", r, n) m = (r, lookupOr0 m r - n)
applyOp (other, _, _) _ = error $ "cant handle op '" ++ other ++ "'."

interpret :: Regs -> [Expr] -> [Int] -> (Regs, [Int])
interpret r [] rs = (r, rs)
interpret r ((o, c):exs) rs
  | checkCondition c r = interpret (M.insert reg n r) exs (n:rs)
  | otherwise = interpret r exs rs
  where
    (reg, n) = applyOp o r

commonPart :: String -> (Regs, [Int])
commonPart ip = interpret M.empty expr []
  where
    expr = case P.parse exprs "error" ip of
            Right p -> p
            Left e -> error $ show e

part1 :: String -> Int
part1 input = maximum $ M.elems m
  where
    (m, _) = commonPart input

part2 :: [Char] -> Int
part2 input = maximum rs
  where
    (_, rs) = commonPart input
