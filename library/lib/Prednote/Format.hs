{-# LANGUAGE OverloadedStrings #-}
-- | Functions used to format text.  Typically you won't need these
-- unless you want tailored control over how your 'Prednote.Core.Pred'
-- are formatted.
module Prednote.Format where

import Rainbow
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid

-- # Labels and indentation

indentAmt :: Int
indentAmt = 2

-- | A colorful label for 'True' values.
lblTrue :: [Chunk]
lblTrue = [fore green <> "TRUE"]

-- | A colorful label for 'False' values.
lblFalse :: [Chunk]
lblFalse = [fore red <> "FALSE"]

-- | Prefixes the given list of 'Chunk' with colorful text to indicate
-- 'True' or 'False' as appropriate.  Appends a newline.
lblLine
  :: Int
  -- ^ Indentation
  -> Bool
  -> [Chunk]
  -> [Chunk]
lblLine idt tf cs = (spaces : lbl) ++ (" " : cs) ++ ["\n"]
  where
    lbl | tf = lblTrue
        | otherwise = lblFalse
    spaces = fromText (X.replicate (indentAmt * idt) (X.singleton ' '))

-- | Indents the given list of 'Chunk' by the given 'Int' multipled by
-- 'indentAmt'.  Appends a newline.
indent :: Int -> [Chunk] -> [Chunk]
indent i cs = spaces : cs ++ [fromText "\n"]
  where
    spaces = fromText . X.replicate (indentAmt * i)
      . X.singleton $ ' '

-- | Append two 'Text', with an intervening space if both 'Text' are
-- not empty.
(<+>) :: Text -> Text -> Text
l <+> r
  | full l && full r = l <> " " <> r
  | otherwise = l <> r
  where
    full = Prelude.not . X.null

