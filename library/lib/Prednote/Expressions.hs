{-# LANGUAGE OverloadedStrings #-}

-- | Handles parsing of both infix and RPN Predbox expressions.
module Prednote.Expressions
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
import qualified Prednote.Expressions.Infix as I
import qualified Prednote.Expressions.RPN as R
import Prednote.Core

-- | A single type for both RPN tokens and infix tokens.
newtype Token a = Token { unToken :: I.InfixToken a }

type Error = X.Text

-- | Creates Operands from Predbox.
operand :: Pred a -> Token a
operand p = Token (I.TokRPN (R.TokOperand p))

-- | The And operator
opAnd :: Token a
opAnd = Token (I.TokRPN (R.TokOperator R.OpAnd))

-- | The Or operator
opOr :: Token a
opOr = Token (I.TokRPN (R.TokOperator R.OpOr))

-- | The Not operator
opNot :: Token a
opNot = Token (I.TokRPN (R.TokOperator R.OpNot))

-- | Open parentheses
openParen :: Token a
openParen = Token (I.TokParen I.Open)

-- | Close parentheses
closeParen :: Token a
closeParen = Token (I.TokParen I.Close)

-- | Is this an infix or RPN expression?
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

-- | Parses expressions. Fails if the expression is nonsensical in
-- some way (for example, unbalanced parentheses, parentheses in an
-- RPN expression, or multiple stack values remaining.) Works by first
-- changing infix expressions to RPN ones.
parseExpression
  :: ExprDesc
  -> [Token a]
  -> Either Error (Pred a)
parseExpression e toks = do
  rpnToks <- case e of
    Infix -> maybe (Left "unbalanced parentheses\n") Right
             . I.createRPN
             . map unToken
             $ toks
    RPN -> maybe (Left "parentheses in an RPN expression\n") Right
           $ toksToRPN toks
  R.parseRPN rpnToks
