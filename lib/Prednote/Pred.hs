{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Prednote.Pred where

import System.Console.Rainbow
import Prednote.Pred.Core
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid
import Prelude hiding (and, or, not, compare)
import qualified Prelude

-- # Labels

lblTrue :: [Chunk]
lblTrue = ["[", f_green <> "TRUE", "]"]

lblFalse :: [Chunk]
lblFalse = ["[", f_red <> "FALSE", "]"]

shortCircuit :: Int -> [Chunk]
shortCircuit = indent ["[", f_yellow <> "short circuit", "]"]

(<+>) :: Text -> Text -> Text
l <+> r
  | full l && full r = l <> " " <> r
  | otherwise = l <> r
  where
    full = Prelude.not . X.null

(<++>) :: [Chunk] -> [Chunk] -> [Chunk]
l <++> r
  | full l && full r = l ++ [" "] ++ r
  | otherwise = l ++ r
  where
    full = (/= 0) . sum . map (sum . map X.length . text)


-- # Indentation

indentAmt :: Int
indentAmt = 2

indent :: [Chunk] -> Int -> [Chunk]
indent cs i = s : cs
  where
    s = fromText . X.replicate (i * indentAmt) . X.singleton $ ' '

indentedReport :: Bool -> Text -> Int -> [Chunk]
indentedReport tf txt = indent (trueFalse <++> [fromText txt])
  where
    trueFalse | tf = lblTrue
              | otherwise = lblFalse

indentedLbl :: Text -> Int -> [Chunk]
indentedLbl = indent . (:[]) . fromText

-- # Visibility

newtype Visible = Visible { unVisible :: Bool }
  deriving (Eq, Ord, Show)

reveal :: Pred a -> Pred a
reveal (Pred l c) = Pred l c'
  where
    c' = case c of
      Predicate f -> Predicate $ \a -> (f a) { visible = True }
      Single p f -> Single p (\a b -> (f a b) { visible = True })
      Variable ps f -> Variable ps (\a bs ->
        let r = f a bs in r { output = (output r) { visible = True } } )
      Fan get p f -> Fan get p (\a bs ->
        let r = f a bs in r { output = (output r) { visible = True } })


hide :: Pred a -> Pred a
hide (Pred l c) = Pred l c'
  where
    c' = case c of
      Predicate f -> Predicate $ \a -> (f a) { visible = False }
      Single p f -> Single p (\a b -> (f a b) { visible = False })
      Variable ps f -> Variable ps (\a bs ->
        let r = f a bs in r { output = (output r) { visible = False } } )
      Fan get p f -> Fan get p (\a bs ->
        let r = f a bs in r { output = (output r) { visible = False } })


-- # Predicate

predicate
  :: Text
  -- ^ Static label
  -> (a -> (Bool, Visible, Text))
  -- ^ Predicate function
  -> Pred a
predicate lbl pdct = Pred l c
  where
    l = indentedLbl lbl
    c = Predicate f
    f a = Output vis res rpt
      where
        (res, (Visible vis), dyn) = pdct a
        rpt = indentedReport res dyn

-- # Conjunction and disjunction, negation

-- | No child 'Pred' may be 'False'.  An empty list of child 'Pred'
-- returns 'True'.
and :: (Bool -> Bool) -> [Pred a] -> Pred a
and shw preds = Pred (indentedLbl lbl) c
  where
    c = Variable preds f
    f _ = go 1
    go !i ls = case ls of
      [] -> Shortable Nothing (Output (shw True) True rpt)
        where
          rpt = indentedReport True lbl
      x:xs
        | x -> go (i + 1) xs
        | otherwise -> Shortable (Just (i, shortCircuit)) out
        where
          out = Output (shw x) x (indentedReport x lbl)
    lbl = "and"

-- | Creates 'and' 'Pred' that are always shown.
(&&&) :: Pred a -> Pred a -> Pred a
l &&& r = and (const True) [l, r]

infixr 3 &&&

-- | At least one child 'Pred' must be 'True'.  An empty list of child
-- 'Pred' returns 'False'.

or :: (Bool -> Bool) -> [Pred a] -> Pred a
or shw preds = Pred (indentedLbl lbl) c
  where
    lbl = "or"
    c = Variable preds f
    f _ = go 1
    go !i ls = case ls of
      [] -> Shortable Nothing (Output (shw False) False rpt)
        where
          rpt = indentedReport False lbl
      x:xs
        | Prelude.not x -> go (i + 1) xs
        | otherwise -> Shortable (Just (i, shortCircuit)) out
        where
          out = Output (shw x) x (indentedReport x lbl)

-- | Creates 'or' 'Pred' that are always shown.
(|||) :: Pred a -> Pred a -> Pred a
l ||| r = or (const True) [l, r]

infixr 2 |||

not :: (Bool -> Bool) -> Pred a -> Pred a
not shw pdct = Pred (indentedLbl lbl) c
  where
    lbl = "not"
    c = Single pdct f
    f _ b = Output (shw res) res (indentedReport res lbl)
      where
        res = Prelude.not b


-- # Fanout

-- | No child item may be 'False'.  An empty list of child items
-- returns 'True'.
fanand
  :: Text
  -- ^ Static label

  -> (Bool -> Bool)
  -- ^ Determines visibility

  -> (a -> ([b], Text))
  -- ^ When applied to a single item, this function returns a list of
  -- items and a dynamic label.

  -> Pred b
  -> Pred a
fanand stl vis get pdct = Pred (indentedLbl stl) c
  where
    c = Fan (fmap fst get) pdct fn
    fn a bs = Shortable shrt (Output vsble res rpt)
      where
        rpt = indentedLbl . snd . get $ a
        (shrt, vsble, res) = go 1 bs
        go !i bools = case bools of
          [] -> (Nothing, vis True, True)
          x:xs
            | Prelude.not x -> (ss, vis False, False)
            | otherwise -> go (i + 1) xs
            where
              ss = Just (i, shortCircuit)


-- | At least one child item must be 'True'.  An empty list of child
-- items returns 'False'.
fanor
  :: Text
  -- ^ Static label

  -> (Bool -> Bool)
  -- ^ Determines visibility

  -> (a -> ([b], Text))
  -- ^ When applied to a single item, this function returns a list of
  -- items and a dynamic label.

  -> Pred b
  -> Pred a
fanor stl vis get pdct = Pred (indentedLbl stl) c
  where
    c = Fan (fmap fst get) pdct fn
    fn a bs = Shortable shrt (Output vsble res rpt)
      where
        rpt = indentedLbl . snd . get $ a
        (shrt, vsble, res) = go 1 bs
        go !i bools = case bools of
          [] -> (Nothing, vis False, False)
          x:xs
            | x -> (ss, vis True, True)
            | otherwise -> go (i + 1) xs
            where
              ss = Just (i, shortCircuit)


-- | At least the given number of child items must be 'True'.
fanAtLeast
  :: Int
  -- ^ At least this many child items must be 'True'.  No matter what
  -- number you supply here, 'fanAtLeast' will return 'True' only if
  -- it finds at least one item.  That is, even if you supply zero or
  -- a negative number, you will still get 'False' if no child items
  -- match.

  -> Text
  -- ^ Static label

  -> (Bool -> Bool)
  -- ^ Determines visibility

  -> (a -> ([b], Text))
  -- ^ When applied to a single item, this function returns a list of
  -- items and a dynamic label.

  -> Pred b
  -> Pred a
fanAtLeast atLeast stl vis get pdct = Pred (indentedLbl stl) c
  where
    c = Fan (fmap fst get) pdct fn
    fn a bs = Shortable shrt (Output vsble res rpt)
      where
        rpt = indentedLbl . snd . get $ a
        (shrt, vsble, res) = go 1 0 bs
        go !i !nFound bools = case bools of
          [] -> (Nothing, vis False, False)
          x:xs
            | x -> if nFound + 1 >= atLeast
                   then (ss, vis True, True)
                   else go (i + 1) (nFound + 1) xs
            | otherwise -> go (i + 1) nFound xs
            where
              ss = Just (i, shortCircuit)

-- # Comparisons

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareBy
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> (a -> (Ordering, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return LT if the item is less than the right
  -- hand side; GT if greater; EQ if equal to the right hand side.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a

compareBy typeDesc rhsDesc ord get = predicate stat fn
  where
    stat = typeDesc <+> "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    fn a = (res, Visible True, dyn)
      where
        (ord', txt) = get a
        res = ord' == ord
        dyn = typeDesc <+> txt <+> "is" <+> ordDesc <+> rhsDesc

-- | Overloaded version of 'compareBy'.

compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> a

  -> Pred a

compare typeDesc rhsDesc ord rhs = compareBy typeDesc rhsDesc ord fn
  where
    fn lhs = (Prelude.compare lhs rhs, X.pack . show $ lhs)

-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybe
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> (a -> (Maybe Ordering, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return LT if the item is less than the right
  -- hand side; GT if greater; EQ if equal to the right hand side.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a

compareByMaybe typeDesc rhsDesc ord get = predicate stat fn
  where
    stat = typeDesc <+> "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    fn a = (res, Visible True, dyn)
      where
        (ord', txt) = get a
        res = case ord' of
          Nothing -> False
          Just o -> o == ord
        dyn = typeDesc <+> txt <+> "is" <+> ordDesc <+> rhsDesc

greater
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched
  -> Text
  -- ^ Description of the right-hand side
  -> a
  -> Pred a
greater typeDesc rhsDesc = compare typeDesc rhsDesc GT


less
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched
  -> Text
  -- ^ Description of the right-hand side
  -> a
  -> Pred a
less typeDesc rhsDesc = compare typeDesc rhsDesc GT


