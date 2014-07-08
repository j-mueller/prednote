{-# LANGUAGE OverloadedStrings, BangPatterns #-}

-- | Functions to work with 'Pred'.  This module works with 'Text' and
-- produces 'Pred' that make sparing use of color.  For more control
-- over the 'Pred' produced, use "Prednote.Pred.Core".
--
-- Exports some names that conflict with Prelude names, so you might
-- want to do something like
--
-- > import qualified Prednote.Pred as P
module Prednote.Pred
  ( 
  -- * Predicates
    C.Pred(..)

  -- * Visibility
  , C.Visible(..)
  , C.shown
  , C.hidden
  , reveal
  , hide
  , showTrue
  , showFalse

  -- * Predicate creation
  , predicate
  , true
  , false

  -- * Conjunction, disjunction, negation
  , all
  , (&&&)
  , any
  , (|||)
  , not

  -- * Fan
  , fanAll
  , fanAny
  , fanAtLeast

  -- * Display and evaluation
  , C.report
  , C.plan
  , C.test
  , C.testV
  , C.filter
  , C.filterV

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

  -- * Labels and indentation
  , lblTrue
  , lblFalse
  , indentAmt
  , lblLine
  , indent
  , shortCir
  , indentTxt
  , (<+>)

  -- * Naming and changing output
  , rename
  , changeOutput
  , speak
  , speakShort
  )where


import qualified Prednote.Pred.Core as C
import qualified Data.Tree as E
import qualified Data.Text as X
import Data.Text (Text)
import System.Console.Rainbow
import Data.Monoid
import Prelude hiding (and, or, not, filter, compare, any, all)
import qualified Prelude

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

-- # Predicate

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

-- | Builds predicates.
predicate
  :: Text
  -- ^ Static label

  -> (a -> Text)
  -- ^ Computes the dynamic label.  Do not indicate whether the result
  -- is 'True' or 'False'; this is automatically done for you.

  -> (a -> Bool)
  -- ^ Predicate function

  -> C.Pred a

predicate r s f = rename r . speak s $ C.Pred (E.Node (const []) []) ev
  where
    ev a = E.Node (C.Output (f a) C.shown Nothing (const [])) []

-- | Always returns 'True' and is always visible.
true :: C.Pred a
true = predicate l (const l) (const True)
  where
    l = "always True"

-- | Always returns 'False' and is always visible.
false :: C.Pred a
false = predicate l (const l) (const False)
  where
    l = "always False"

-- # Visibility

-- | Creates a 'C.Pred' with its visibility modified.
visibility
  :: (Bool -> C.Visible)
  -- ^ When applied to the 'C.result' of the 'C.Pred', this function
  -- returns the desired visibility.
  -> C.Pred a
  -> C.Pred a
visibility f (C.Pred s e) = C.Pred s e'
  where
    e' a = g (e a)
    g (E.Node n cs) = E.Node n { C.visible = f (C.result n) } cs

-- | Creates a 'C.Pred' that is always shown.
reveal :: C.Pred a -> C.Pred a
reveal = visibility (const C.shown)

-- | Creates a 'C.Pred' that is always hidden.
hide :: C.Pred a -> C.Pred a
hide = visibility (const C.hidden)

-- | Creates a 'C.Pred' that is shown only if its 'C.result' is
-- 'True'.
showTrue :: C.Pred a -> C.Pred a
showTrue = visibility (\b -> if b then C.shown else C.hidden)

-- | Creates a 'C.Pred' that is shown only if its 'C.result' is
-- 'False'.
showFalse :: C.Pred a -> C.Pred a
showFalse = visibility (\b -> if Prelude.not b then C.shown else C.hidden)

-- # Conjunction and disjunction, negation

-- | No child 'Pred' may be 'False'.  An empty list of child 'Pred'
-- returns 'True'.  Always visible.
all :: [C.Pred a] -> C.Pred a
all = speakShort . rename l . speak (const l) . C.all
  where
    l = "all"

-- | Creates 'all' 'Pred' that are always visible.
(&&&) :: C.Pred a -> C.Pred a -> C.Pred a
l &&& r = all [l, r]

infixr 3 &&&

-- | At least one child 'Pred' must be 'True'.  An empty list of child
-- 'Pred' returns 'False'.  Always visible.
any :: [C.Pred a] -> C.Pred a
any = speakShort . rename l . speak (const l) . C.any
  where
    l = "any"


-- | Creates 'any' 'Pred' that are always visible.
(|||) :: C.Pred a -> C.Pred a -> C.Pred a
l ||| r = any [l, r]

infixr 2 |||

-- | Negation.  Always visible.
not :: C.Pred a -> C.Pred a
not = rename l . speak (const l) . C.not
  where
    l = "not"

-- | No fanned-out item may be 'False'.  An empty list of child items
-- returns 'True'.
fanAll :: (a -> [b]) -> C.Pred b -> C.Pred a
fanAll f = speakShort . rename l . speak (const l) . C.fanAll f
  where
    l = "fanout all"



-- | At least one fanned-out item must be 'True'.  An empty list of
-- child items returns 'False'.
fanAny :: (a -> [b]) -> C.Pred b -> C.Pred a
fanAny f = speakShort . rename l . speak (const l) . C.fanAny f
  where
    l = "fanout any"


-- | At least the given number of child items must be 'True'.
fanAtLeast :: Int -> (a -> [b]) -> C.Pred b -> C.Pred a
fanAtLeast i f = speakShort . rename l . speak (const l)
  . C.fanAtLeast i f
  where
    l = "fanout - at least " <> X.pack (show i) <>
      " fanned-out subject(s) must be True"

-- # Comparisons

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareBy
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare the left-hand side to the right-hand side.
  -- Return LT if the item is less than the right hand side; GT if
  -- greater; EQ if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> C.Pred a

compareBy typeDesc rhsDesc lhsDesc get ord = predicate stat dyn pd
  where
    stat = typeDesc <+> "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    dyn a = typeDesc <+> lhsDesc a <+> "is" <+> ordDesc <+> rhsDesc
    pd a = get a == ord

-- | Overloaded version of 'compareBy'.

compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> C.Pred a
compare typeDesc rhs ord =
  compareBy typeDesc (X.pack . show $ rhs) (X.pack . show)
            (`Prelude.compare` rhs) ord

-- | Builds a 'Pred' that tests items for equality.

equalBy
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if the items are equal; 'False' otherwise.

  -> C.Pred a
equalBy typeDesc rhsDesc lhsDesc get = predicate stat dyn get
  where
    stat = typeDesc <+> "is equal to" <+> rhsDesc
    dyn a = typeDesc <+> lhsDesc a <+> "is equal to" <+> rhsDesc

-- | Overloaded version of 'equalBy'.

equal
  :: (Eq a, Show a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
equal typeDesc rhs = equalBy typeDesc (X.pack . show $ rhs)
                             (X.pack . show) (== rhs)


-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybe
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Maybe Ordering)
  -- ^ How to compare an item against the right hand side. Return LT if
  -- the item is less than the right hand side; GT if greater; EQ if
  -- equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> C.Pred a

compareByMaybe typeDesc rhsDesc lhsDesc get ord = predicate stat dyn fn
  where
    stat = typeDesc <+> "is" <+> ordDesc <+> rhsDesc
    dyn a = typeDesc <+> lhsDesc a <+> "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    fn a = case get a of
      Nothing -> False
      Just o -> o == ord

greater
  :: (Show a, Ord a)

  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
greater typeDesc rhs = compare typeDesc rhs GT

less
  :: (Show a, Ord a)

  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
less typeDesc rhs = compare typeDesc rhs GT

greaterEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
greaterEq t r = greater t r ||| equal t r

lessEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
lessEq t r = less t r ||| equal t r

notEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
notEq t r = not $ equal t r

greaterBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
greaterBy dT dR dL get = compareBy dT dR dL get GT


lessBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
lessBy dT dR dL get = compareBy dT dR dL get LT

greaterEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
greaterEqBy dT dR dL f = greaterBy dT dR dL f ||| equalBy dT dR dL f'
  where
    f' = fmap (== EQ) f

lessEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
lessEqBy dT dR dL f = lessBy dT dR dL f ||| equalBy dT dR dL f'
  where
    f' = fmap (== EQ) f

notEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if equal; 'False' otherwise.

  -> C.Pred a
notEqBy dT dR dL = not . equalBy dT dR dL


-- | Parses a string that contains text, such as @>=@, which indicates
-- which comparer to use.  Returns the comparer.
parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed

  -> (Ordering -> C.Pred a)
  -- ^ A function that, when given an ordering, returns a 'C.Pred'.
  -- Typically you will get this by partial application of 'compare',
  -- 'compareBy', or 'compareByMaybe'.

  -> Maybe (C.Pred a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- 'C.Pred'.
parseComparer t f
  | t == ">" = Just (f GT)
  | t == "<" = Just (f LT)
  | t == "=" = Just (f EQ)
  | t == "==" = Just (f EQ)
  | t == ">=" = Just (f GT ||| f EQ)
  | t == "<=" = Just (f LT ||| f EQ)
  | t == "/=" = Just (not $ f EQ)
  | t == "!=" = Just (not $ f EQ)
  | otherwise = Nothing

