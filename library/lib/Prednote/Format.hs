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
lblTrue = ["[", fore green <> "TRUE", "]"]

-- | A colorful label for 'False' values.
lblFalse :: [Chunk]
lblFalse = ["[", fore red <> "FALSE", "]"]

-- | Prefixes the given 'Text' with colorful text to indicate 'True'
-- or 'False' as appropriate.
lblLine :: Bool -> Text -> [Chunk]
lblLine b t = lbl ++ [" ", fromText t]
  where
    lbl | b = lblTrue
        | otherwise = lblFalse

-- | Indents the given list of 'Chunk' by the given 'Int' multipled by
-- 'indentAmt'.  Appends a newline.
indent :: [Chunk] -> Int -> [Chunk]
indent cs i = spaces : cs ++ [fromText "\n"]
  where
    spaces = fromText . X.replicate (indentAmt * i)
      . X.singleton $ ' '

-- | Indents a 'Text' by the given 'Int' multiplied by
-- 'indentAmt'.
indentTxt :: Text -> Int -> [Chunk]
indentTxt = indent . (:[]) . fromText

-- | Append two 'Text', with an intervening space if both 'Text' are
-- not empty.
(<+>) :: Text -> Text -> Text
l <+> r
  | full l && full r = l <> " " <> r
  | otherwise = l <> r
  where
    full = Prelude.not . X.null

