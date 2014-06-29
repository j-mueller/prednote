{-# LANGUAGE OverloadedStrings, BangPatterns #-}

-- | Functions to work with 'Pred'.  This module works with 'Text' and
-- produces 'Pred' that make sparing use of color.  For more control
-- over the 'Pred' produced, use "Prednote.Pred.Core".
--
-- Exports some names that conflict with Prelude names, such as 'and',
-- 'or', 'not', and 'filter'; keep this in mind when you
-- @import@ this module.
module Prednote.Pred where

import qualified Prednote.Pred.Core as C
import Prednote.Pred.Core (Pred)
import qualified Data.Text as X
import Data.Text (Text)
import System.Console.Rainbow
import Data.Monoid
import Prelude hiding (and, or, not, filter)

lblTrue :: [Chunk]
lblTrue = ["[", f_green <> "TRUE", "]"]

lblFalse :: [Chunk]
lblFalse = ["[", f_red <> "FALSE", "]"]

indentAmt :: Int
indentAmt = 2

lblLine :: Bool -> Text -> [Chunk]
lblLine b t = lbl ++ [" ", fromText t]
  where
    lbl | b = lblTrue
        | otherwise = lblFalse

indent :: [Chunk] -> Int -> [Chunk]
indent cs i = spaces : cs ++ [fromText "\n"]
  where
    spaces = fromText . X.replicate (indentAmt * i)
      . X.singleton $ ' '

shortCir :: Int -> [Chunk]
shortCir = indent ["[", f_yellow <> "short circuit" <> "]"]

indentTxt :: Text -> Int -> [Chunk]
indentTxt = indent . (:[]) . fromText

predicate
  :: Text
  -- ^ Static name
  -> (a -> (Bool, Bool, Text))
  -- ^ Compute result.  The first element of the result tuple is the
  -- result itself.  The second is whether the result is visible, and
  -- the third is a label.
  -> Pred a
predicate l f = C.predicate (indentTxt l) f'
  where
    f' a = (r, v, indentTxt dyn)
      where
        (r, v, dyn) = f a

and :: [Pred a] -> Pred a
and = C.and (indentTxt "and") shortCir dyn
  where
    dyn b _ = indent $ lblLine b "and"

(&&&) :: Pred a -> Pred a -> Pred a
l &&& r = and [l, r]

infixr 3 &&&

or :: [Pred a] -> Pred a
or = C.or (indentTxt "or") shortCir dyn
  where
    dyn b _ = indent $ lblLine b "or"

(|||) :: Pred a -> Pred a -> Pred a
l ||| r = or [l, r]

infixr 2 |||

not :: Pred a -> Pred a
not = C.not (indentTxt "not") dyn
  where
    dyn b _ = indent $ lblLine b "not"

fanand :: (a -> [b]) -> Pred b -> Pred a
fanand = C.fanand (indentTxt lbl) shortCir dyn
  where
    lbl = "fanout and - no fanned-out subject may be False"
    dyn b _ = indent $ lblLine b lbl

fanor :: (a -> [b]) -> Pred b -> Pred a
fanor = C.fanor (indentTxt lbl) shortCir dyn
  where
    lbl = "fanout or - one fanned-out subject must be True"
    dyn b _ = indent $ lblLine b lbl

fanAtLeast :: Int -> (a -> [b]) -> Pred b -> Pred a
fanAtLeast i = C.fanAtLeast i (indentTxt lbl) shortCir dyn
  where
    lbl = "fanout at least - at least " <> X.pack (show i)
      <> " fanned-out subject(s) must be True"
    dyn b _ = indent $ lblLine b lbl
