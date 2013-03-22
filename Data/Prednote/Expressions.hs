{-# LANGUAGE OverloadedStrings #-}
module Data.Prednote.Expressions
  ( ExprDesc(..)
  , Error
  , Token
  , operand
  , opAnd
  , opOr
  , opNot
  , openParen
  , closeParen
  , parseExpression
  ) where

import Data.Either (partitionEithers)
import qualified Data.Text as X
import qualified Data.Prednote.Expressions.Infix as I
import qualified Data.Prednote.Expressions.RPN as R
import qualified Data.Prednote.Pdct as P
import qualified Control.Monad.Exception.Synchronous as Ex

-- | A single type for both RPN tokens and infix tokens.
newtype Token a = Token { unToken :: I.InfixToken a }
type Error = X.Text

operand :: P.Pdct a -> Token a
operand p = Token (I.TokRPN (R.TokOperand p))

opAnd :: Token a
opAnd = Token (I.TokRPN (R.TokOperator R.OpAnd))

opOr :: Token a
opOr = Token (I.TokRPN (R.TokOperator R.OpOr))

opNot :: Token a
opNot = Token (I.TokRPN (R.TokOperator R.OpNot))

openParen :: Token a
openParen = Token (I.TokParen I.Open)

closeParen :: Token a
closeParen = Token (I.TokParen I.Close)

data ExprDesc
  = Infix
  | RPN
  deriving (Eq, Show)

toksToRPN :: [Token a] -> Maybe [R.RPNToken a]
toksToRPN toks
  = let toEither t = case unToken t of
          I.TokRPN tok -> Right tok
          _ -> Left ()
    in case partitionEithers . map toEither $ toks of
        ([], xs) -> return xs
        _ -> Nothing

parseExpression
  :: ExprDesc
  -> [Token a]
  -> Ex.Exceptional Error (P.Pdct a)
parseExpression e toks = do
  rpnToks <- case e of
    Infix -> Ex.fromMaybe "unbalanced parentheses\n"
             . I.createRPN
             . map unToken
             $ toks
    RPN -> Ex.fromMaybe "parentheses in an RPN expression\n"
           $ toksToRPN toks
  R.pushTokens rpnToks
