{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Prednote.Pred
  (
  -- * Predicates
    Pred

  -- * Visibility
  , Visible(..)
  , reveal
  , hide

  -- * Creation of 'Pred'
  , predicate
  , and
  , (&&&)
  , or
  , (|||)
  , not
  , fanand
  , fanor
  , fanAtLeast

  -- * Comparisons - overloaded
  , compare
  , equal
  , greater
  , less
  , greaterEq
  , lessEq
  , notEq

  -- * Comparisons - not overloaded
  , compareBy
  , equalBy
  , compareByMaybe
  , greaterBy
  , lessBy
  , greaterEqBy
  , lessEqBy
  , notEqBy

  -- * Comparers
  , parseComparer
  ) where

import Control.Arrow (first)
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

-- | Builds a 'Pred' that tests items for equality.

equalBy
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> (Bool, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return True if the items are equal; False otherwise.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a
equalBy typeDesc rhsDesc get = predicate stat fn
  where
    stat = typeDesc <+> "is equal to" <+> rhsDesc
    fn a = (res, Visible True, dyn)
      where
        (res, txt) = get a
        dyn = typeDesc <+> txt <+> "is equal to" <+> rhsDesc

-- | Overloaded version of 'equalBy'.

equal
  :: (Eq a, Show a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> Pred a
equal typeDesc rhs = equalBy typeDesc (X.pack . show $ rhs) f
  where
    f lhs = (lhs == rhs, X.pack . show $ lhs)

-- | Overloaded version of 'compareBy'.

compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> a

  -> Pred a

compare typeDesc ord rhs = compareBy typeDesc rhsDesc ord fn
  where
    fn lhs = (Prelude.compare lhs rhs, X.pack . show $ lhs)
    rhsDesc = X.pack . show $ rhs

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
  -> a
  -> Pred a
greater typeDesc = compare typeDesc GT


less
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched
  -> a
  -> Pred a
less typeDesc = compare typeDesc GT

greaterEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pred a
greaterEq t r = greater t r ||| equal t r

lessEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pred a
lessEq t r = less t r ||| equal t r

notEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pred a
notEq t r = not (const True) $ equal t r

greaterBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> (Ordering, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return LT if the item is less than the right
  -- hand side; GT if greater; EQ if equal to the right hand side.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a
greaterBy dT dR f = compareBy dT dR GT f


lessBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> (Ordering, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return LT if the item is less than the right
  -- hand side; GT if greater; EQ if equal to the right hand side.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a
lessBy dT dR f = compareBy dT dR LT f

greaterEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> (Ordering, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return LT if the item is less than the right
  -- hand side; GT if greater; EQ if equal to the right hand side.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a
greaterEqBy dT dR f = greaterBy dT dR f ||| equalBy dT dR f'
  where
    f' = fmap (first (== EQ)) f

lessEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> (Ordering, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return LT if the item is less than the right
  -- hand side; GT if greater; EQ if equal to the right hand side.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a
lessEqBy dT dR f = lessBy dT dR f ||| equalBy dT dR f'
  where
    f' = fmap (first (== EQ)) f

notEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> (Bool, Text))
  -- ^ The first of the pair is how to compare an item against the
  -- right hand side. Return True if the items are equal; False if not.
  --
  -- The second of the pair is a description of the left hand side.

  -> Pred a
notEqBy dT dR f = not (const True) $ equalBy dT dR f


parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed
  -> (Ordering -> Pred a)
  -- ^ A function that, when given an ordering, returns a Predbox
  -> Maybe (Pred a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- Predbox.
parseComparer t f
  | t == ">" = Just (f GT)
  | t == "<" = Just (f LT)
  | t == "=" = Just (f EQ)
  | t == "==" = Just (f EQ)
  | t == ">=" = Just (f GT ||| f EQ)
  | t == "<=" = Just (f LT ||| f EQ)
  | t == "/=" = Just (not (const True) $ f EQ)
  | t == "!=" = Just (not (const True) $ f EQ)
  | otherwise = Nothing



