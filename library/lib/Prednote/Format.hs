{-# LANGUAGE OverloadedStrings #-}
module Prednote.Format where

import System.Console.Rainbow
import Data.Text (Text)
import qualified Data.Text as X
import qualified Prednote.Core as C
import qualified Data.Tree as E
import Data.Monoid

-- # Labels and indentation

-- | A colorful label for 'True' values.
lblTrue :: [Chunk]
lblTrue = ["[", f_green <> "TRUE", "]"]

-- | A colorful label for 'False' values.
lblFalse :: [Chunk]
lblFalse = ["[", f_red <> "FALSE", "]"]

-- | Indent amount.
indentAmt :: Int
indentAmt = 2

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

-- | A label for a short circuit.
shortCir :: Int -> [Chunk]
shortCir = indent ["[", f_yellow <> "short circuit" <> "]"]

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

-- | Create a new 'C.Pred' with a different static label.
rename :: Text -> C.Pred a -> C.Pred a
rename x p = p { C.static = (C.static p)
  { E.rootLabel = indentTxt x } }

-- | Creates a new 'C.Pred' with a result differing from the original
-- 'C.Pred'.
changeOutput
  :: (a -> C.Output -> C.Output)
  -- ^ Function to modify the 'C.Output'

  -> C.Pred a
  -- ^ Modify the 'C.Output' of this 'C.Pred'

  -> C.Pred a
changeOutput f p = p { C.evaluate = e' }
  where
    e' a = t'
      where
        t = C.evaluate p a
        t' = t { E.rootLabel = f a (E.rootLabel t) }

-- | Creates a new 'C.Pred' with a different dynamic label.
speak
  :: (a -> Text)
  -- ^ New dynamic label.  Do not indicate whether the result is
  -- 'True' or 'False'; this is done for you.

  -> C.Pred a

  -> C.Pred a
speak f = changeOutput g
  where
    g a o = o { C.dynamic = dyn }
      where
        dyn = indent $ lblLine (C.result o) (f a)


-- | Creates a new 'C.Pred' with any short circuits having a colorful
-- label.
speakShort :: C.Pred a -> C.Pred a
speakShort p = p { C.evaluate = e' }
  where
    e' a = t { E.rootLabel = (E.rootLabel t)
      { C.short = fmap (const shortCir) shrt } }
      where
        t = C.evaluate p a
        shrt = C.short . E.rootLabel $ t

