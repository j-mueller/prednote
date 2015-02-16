{-# LANGUAGE OverloadedStrings #-}

-- | Handles parsing of both infix and RPN 'Pred' expressions.
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
import qualified Prelude
import Prelude hiding (maybe)

-- | A single type for both RPN tokens and infix tokens.
newtype Token m a = Token { unToken :: I.InfixToken m a }

type Error = X.Text

-- | Creates Operands from 'Pred'.
operand :: PredM m a -> Token m a
operand p = Token (I.TokRPN (R.TokOperand p))

-- | The And operator
opAnd :: Token m a
opAnd = Token (I.TokRPN (R.TokOperator R.OpAnd))

-- | The Or operator
opOr :: Token m a
opOr = Token (I.TokRPN (R.TokOperator R.OpOr))

-- | The Not operator
opNot :: Token m a
opNot = Token (I.TokRPN (R.TokOperator R.OpNot))

-- | Open parentheses
openParen :: Token m a
openParen = Token (I.TokParen I.Open)

-- | Close parentheses
closeParen :: Token m a
closeParen = Token (I.TokParen I.Close)

-- | Is this an infix or RPN expression?
data ExprDesc
  = Infix
  | RPN
  deriving (Eq, Show)

toksToRPN :: [Token m a] -> Maybe [R.RPNToken m a]
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
  :: (Functor m, Monad m)
  => ExprDesc
  -> [Token m a]
  -> Either Error (PredM m a)
parseExpression e toks = do
  rpnToks <- case e of
    Infix -> Prelude.maybe (Left "unbalanced parentheses\n") Right
             . I.createRPN
             . map unToken
             $ toks
    RPN -> Prelude.maybe (Left "parentheses in an RPN expression\n") Right
           $ toksToRPN toks
  R.parseRPN rpnToks
