{-# LANGUAGE OverloadedStrings #-}
-- | Postfix, or RPN, expression parsing.
--
-- This module parses RPN expressions where the operands are
-- predicates and the operators are one of @and@, @or@, or @not@,
-- where @and@ and @or@ are binary and @not@ is unary.
module Data.Prednote.Expressions.RPN where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import qualified Data.Prednote.Pdct as P
import Data.Prednote.Pdct ((&&&), (|||))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import qualified System.Console.Rainbow as C

type Error = Text

data RPNToken a
  = TokOperand (P.Pdct a)
  | TokOperator Operator

data Operator
  = OpAnd
  | OpOr
  | OpNot
  deriving Show

pushOperand :: P.Pdct a -> [P.Pdct a] -> [P.Pdct a]
pushOperand p ts = p : ts

pushOperator
  :: Operator
  -> [P.Pdct a]
  -> Ex.Exceptional Error [P.Pdct a]
pushOperator o ts = case o of
  OpAnd -> case ts of
    x:y:zs -> return $ (y &&& x) : zs
    _ -> Ex.throw $ err "and"
  OpOr -> case ts of
    x:y:zs -> return $ (y ||| x) : zs
    _ -> Ex.throw $ err "or"
  OpNot -> case ts of
    x:zs -> return $ P.not x : zs
    _ -> Ex.throw $ err "not"
  where
    err x = "insufficient operands to apply \"" <> x
            <> "\" operator\n"

pushToken
  :: [P.Pdct a]
  -> RPNToken a
  -> Ex.Exceptional Error [P.Pdct a]
pushToken ts t = case t of
  TokOperand p -> return $ pushOperand p ts
  TokOperator o -> pushOperator o ts

pushTokens
  :: Fdbl.Foldable f
  => f (RPNToken a)
  -> Ex.Exceptional Error (P.Pdct a)
pushTokens ts = do
  trees <- Fdbl.foldlM pushToken [] ts
  case trees of
    [] -> Ex.throw $ "bad expression: no operands left on the stack\n"
    x:[] -> return x
    xs -> Ex.throw
      $ "bad expression: multiple operands left on the stack:\n"
      <> ( X.concat
           . map C.chunkText
           . concatMap (P.showPdct 4 0)
           $ xs )

