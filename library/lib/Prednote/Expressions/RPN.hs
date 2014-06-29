{-# LANGUAGE OverloadedStrings #-}
-- | Postfix, or RPN, expression parsing.
--
-- This module parses RPN expressions where the operands are
-- predicates and the operators are one of @and@, @or@, or @not@,
-- where @and@ and @or@ are binary and @not@ is unary.
module Prednote.Expressions.RPN where

import Data.Functor.Contravariant
import qualified Data.Foldable as Fdbl
import qualified Prednote.Pred as P
import Prednote.Pred ((&&&), (|||))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X

data RPNToken a
  = TokOperand (P.Pred a)
  | TokOperator Operator

instance Contravariant RPNToken where
  contramap f t = case t of
    TokOperand p -> TokOperand . contramap f $ p
    TokOperator o -> TokOperator o

data Operator
  = OpAnd
  | OpOr
  | OpNot
  deriving Show

pushOperand :: P.Pred a -> [P.Pred a] -> [P.Pred a]
pushOperand p ts = p : ts

pushOperator
  :: Operator
  -> [P.Pred a]
  -> Either Text [P.Pred a]
pushOperator o ts = case o of
  OpAnd -> case ts of
    x:y:zs -> return $ (y &&& x) : zs
    _ -> Left $ err "and"
  OpOr -> case ts of
    x:y:zs -> return $ (y ||| x) : zs
    _ -> Left $ err "or"
  OpNot -> case ts of
    x:zs -> return $ P.not (const True) x : zs
    _ -> Left $ err "not"
  where
    err x = "insufficient operands to apply \"" <> x
            <> "\" operator\n"

pushToken
  :: [P.Pred a]
  -> RPNToken a
  -> Either Text [P.Pred a]
pushToken ts t = case t of
  TokOperand p -> return $ pushOperand p ts
  TokOperator o -> pushOperator o ts


-- | Parses an RPN expression and returns the resulting Predbox. Fails if
-- there are no operands left on the stack or if there are multiple
-- operands left on the stack; the stack must contain exactly one
-- operand in order to succeed.
parseRPN
  :: Fdbl.Foldable f
  => f (RPNToken a)
  -> Either Text (P.Pred a)
parseRPN ts = do
  trees <- Fdbl.foldlM pushToken [] ts
  case trees of
    [] -> Left $ "bad expression: no operands left on the stack\n"
    x:[] -> return x
    xs -> Left . X.pack
      $ "bad expression: multiple operands left on the stack:\n"
      <> concatMap show xs

