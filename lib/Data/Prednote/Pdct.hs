{-# LANGUAGE OverloadedStrings #-}

-- | Trees of predicates.
--
-- Exports names which conflict with Prelude names, so you probably
-- want to import this module qualified.

module Data.Prednote.Pdct
  ( -- * The Pdct tree
    Label
  , Pdct(..)
  , Node(..)
  , rename
  , always
  , never

  -- * Creating operands
  , operand

  -- * Creating Pdct from other Pdct
  , and
  , or
  , not
  , neverFalse
  , neverTrue
  , (&&&)
  , (|||)
  , boxPdct
  , boxNode

  -- * Showing and evaluating Pdct
  , Level
  , IndentAmt
  , ShowDiscards
  , showPdct
  , evaluate

  -- * Helpers for building common Pdct
  -- ** Non-overloaded
  , compareBy
  , compareByMaybe
  , greaterBy
  , lessBy
  , equalBy
  , greaterEqBy
  , lessEqBy
  , notEqBy

  -- ** Overloaded
  , compare
  , greater
  , less
  , equal
  , greaterEq
  , lessEq
  , notEq
  , parseComparer

  ) where

import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe, isJust, catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat, mempty)
import Data.String (fromString)
import qualified System.Console.Rainbow as R
import Prelude hiding (not, and, or, compare, filter)
import qualified Prelude

type Label = Text

-- | A tree of predicates.
data Pdct a = Pdct Label (Node a)

instance Show (Pdct a) where
  show = X.unpack
         . X.concat
         . map R._text
         . showPdct 2 0

-- | Renames the top level of the Pdct. The function you pass will be
-- applied to the old name.
rename :: (Text -> Text) -> Pdct a -> Pdct a
rename f (Pdct l n) = Pdct (f l) n

data Node a
  = And [Pdct a]
  -- ^ None of the Pdct in list may be Just False. An empty list or
  -- list with only Nothing is Just True.

  | Or [Pdct a]
  -- ^ At least one of the Pdct in the list must be Just True. An
  -- empty list or list with only Nothing is Just False.

  | Not (Pdct a)
  -- ^ Just True is Just False and vice versa; Nothing remains Nothing.

  | NeverFalse (Pdct a)
  -- ^ Just True if the child is Just True; Nothing otherwise.

  | NeverTrue (Pdct a)
  -- ^ Just False if the child is Just False; Nothing otherwise.

  | Operand (a -> Maybe Bool)
  -- ^ An operand may return Just True or Just False to indicate
  -- success or failure. It may also return Nothing to indicate a
  -- discard.

-- | Given a function that un-boxes values of type b, changes a Node
-- from type a to type b.
boxNode
  :: (b -> a)
  -> Node a
  -> Node b
boxNode f n = case n of
  And ls -> And $ map (boxPdct f) ls
  Or ls -> Or $ map (boxPdct f) ls
  Not o -> Not $ boxPdct f o
  NeverFalse o -> NeverFalse $ boxPdct f o
  NeverTrue o -> NeverTrue $ boxPdct f o
  Operand g -> Operand $ \b -> g (f b)


-- | Given a function that un-boxes values of type b, changes a Pdct
-- from type a to type b.
boxPdct
  :: (b -> a)
  -> Pdct a
  -> Pdct b
boxPdct f (Pdct l n) = Pdct l $ boxNode f n

and :: [Pdct a] -> Pdct a
and = Pdct "and" . And

or :: [Pdct a] -> Pdct a
or = Pdct "or" . Or

not :: Pdct a -> Pdct a
not = Pdct "not" . Not

-- | Creates a new operand. The Pdct is Just True or Just False, never
-- Nothing.
operand :: Text -> (a -> Bool) -> Pdct a
operand t = Pdct t . Operand . fmap Just

-- | Turns an existing Pdct to one that never says False. If the
-- underlying predicate returns Just True, the new Pdct also returns
-- Just True. Otherwise, the Pdct returns Nothing.
neverFalse :: Pdct a -> Pdct a
neverFalse = Pdct "never False" . NeverFalse

-- | Turns an existing Pdct to one that never says True. If the
-- underlying predicate returns Just False, the new Pdct also returns
-- Just False. Otherwise, the Pdct returns Nothing.
neverTrue :: Pdct a -> Pdct a
neverTrue = Pdct "never True" . NeverTrue


-- | Returns a tree that is always True.
always :: Pdct a
always = Pdct "always True" (Operand (const (Just True)))

-- | Returns a tree that is always False.
never :: Pdct a
never = Pdct "always False" (Operand (const (Just False)))

-- | Forms a Pdct using 'and'.
(&&&) :: Pdct a -> Pdct a -> Pdct a
(&&&) x y = Pdct "and" (And [x, y])
infixr 3 &&&

-- | Forms a Pdct using 'or'.
(|||) :: Pdct a -> Pdct a -> Pdct a
(|||) x y = Pdct "or" (Or [x, y])
infixr 2 |||

-- | How many levels of indentation to use. Typically you will start
-- this at zero. It is incremented by one for each level as functions
-- descend through the tree.
type Level = Int

-- | The number of spaces to use for each level of indentation.
type IndentAmt = Int

-- | Indents text, and adds a newline to the end.
indent :: IndentAmt -> Level -> [R.Chunk] -> [R.Chunk]
indent amt lvl cs = idt : (cs ++ [nl])
  where
    idt = fromString (replicate (lvl * amt) ' ')
    nl = fromString "\n"

-- | Creates a plain Chunk from a Text.
plain :: Text -> R.Chunk
plain = R.Chunk mempty

-- | Shows a Pdct tree without evaluating it.
showPdct :: IndentAmt -> Level -> Pdct a -> [R.Chunk]
showPdct amt lvl (Pdct l pd) = case pd of
  And ls -> indent amt lvl [plain l]
            <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Or ls -> indent amt lvl [plain l]
           <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Not t -> indent amt lvl [plain l]
           <> showPdct amt (lvl + 1) t
  NeverFalse t -> indent amt lvl [plain l]
                  <> showPdct amt (lvl + 1) t
  NeverTrue t -> indent amt lvl [plain l]
                 <> showPdct amt (lvl + 1) t
  Operand _ -> indent amt lvl [plain l]


labelBool :: Text -> Maybe Bool -> [R.Chunk]
labelBool t b = [open, trueFalse, close, blank, txt]
  where
    trueFalse = case b of
      Nothing -> "discard" <> R.f_yellow
      Just bl -> if bl
        then "TRUE" <> R.f_green
        else "FALSE" <> R.f_red
    open = "["
    close = "]"
    blank = plain (X.replicate blankLen " ")
    blankLen = X.length "discard"
               - X.length (R._text trueFalse) + 1
    txt = plain t

type ShowDiscards = Bool

--
-- Helpers
--

-- | Build a Pdct that compares items.
compareBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Pdct to be Just True; otherwise it is Just
  -- False. The subject will be on the left hand side.

  -> Pdct a

compareBy itemDesc typeDesc cmp ord = Pdct l (Operand f)
  where
    l = typeDesc <> " is " <> cmpDesc <> " " <> itemDesc
    cmpDesc = case ord of
      LT -> "less than"
      GT -> "greater than"
      EQ -> "equal to"
    f subj = Just $ cmp subj == ord

-- | Like 'compareBy' but allows the comparison of items that may fail
-- to return an ordering.
compareByMaybe
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Maybe Ordering)
  -- ^ How to compare an item against the right hand side. Return Just
  -- LT if the item is less than the right hand side; Just GT if
  -- greater; Just EQ if equal to the right hand side. Return Nothing
  -- if the item cannot return an item to be compared. The result of
  -- the evaluation of the Pdct will then be Nothing.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Pdct to be Just True; otherwise it is Just False,
  -- or Nothing if the subject does not return an ordering. The
  -- subject will be on the left hand side.

  -> Pdct a

compareByMaybe itemDesc typeDesc cmp ord = Pdct l (Operand f)
  where
    l = typeDesc <> " is " <> cmpDesc <> " " <> itemDesc
    cmpDesc = case ord of
      LT -> "less than"
      GT -> "greater than"
      EQ -> "equal to"
    f subj = maybe Nothing (Just . (== ord)) $ cmp subj

-- | Overloaded version of 'compareBy'.
compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ The right hand side of the comparison.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Pdct to be Just True; otherwise it is Just
  -- False. The subject will be on the left hand side.

  -> Pdct a
compare typeDesc a ord = compareBy itemDesc typeDesc cmp ord
  where
    itemDesc = X.pack . show $ a
    cmp item = Prelude.compare item a

greater
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> a
  -- ^ The right hand side of the comparison.

  -> Pdct a
greater d a = compare d a GT

less
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> a
  -- ^ The right hand side of the comparison.

  -> Pdct a
less d a = compare d a LT

equal
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> a
  -- ^ The right hand side of the comparison.

  -> Pdct a
equal d a = compare d a EQ

greaterEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> a
  -- ^ The right hand side of the comparison.

  -> Pdct a
greaterEq d a = greater d a ||| equal d a

lessEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> a
  -- ^ The right hand side of the comparison.

  -> Pdct a
lessEq d a = less d a ||| equal d a

notEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> a
  -- ^ The right hand side of the comparison.

  -> Pdct a
notEq d a = not $ equal d a

greaterBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Pdct a
greaterBy iD tD cmp = compareBy iD tD cmp GT

lessBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Pdct a
lessBy iD tD cmp = compareBy iD tD cmp LT

equalBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Pdct a
equalBy iD tD cmp = compareBy iD tD cmp EQ

greaterEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Pdct a
greaterEqBy iD tD cmp =
  greaterBy iD tD cmp ||| equalBy iD tD cmp

lessEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Pdct a
lessEqBy iD tD cmp =
  lessBy iD tD cmp ||| equalBy iD tD cmp

notEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Pdct

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Pdct a
notEqBy iD tD cmp =
  not $ equalBy iD tD cmp

--
-- Comparer parsers
--

-- | Parses a string to find the correct comparer; returns the correct
-- function to build a Pdct.

parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed
  -> (Ordering -> Pdct a)
  -- ^ A function that, when given an ordering, returns a Pdct
  -> Maybe (Pdct a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- Pdct.
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

--
-- # Result
--

data NeverTrue = NTFalse | NTDiscard
  deriving (Eq, Show, Ord)

data NeverFalse = NFTrue | NFDiscard
  deriving (Eq, Show, Ord)

data Result = Result
  { rLabel :: Label
  , rNode :: RNode
  } deriving (Eq, Show)

data RNode
  = RAnd Bool [Result]
  | ROr Bool [Result]
  | RNot (Maybe Bool) Result
  | RNeverTrue NeverTrue Result
  | RNeverFalse NeverFalse Result
  | ROperand (Maybe Bool)
  deriving (Eq, Show)

isTrue :: RNode -> Bool
isTrue n = case n of
  RAnd b _ -> b
  ROr b _ -> b
  RNot mb _ -> fromMaybe False mb
  RNeverTrue nt _ -> False
  RNeverFalse nf _ -> nf == NFTrue
  ROperand mb -> fromMaybe False mb

isFalse :: RNode -> Bool
isFalse n = case n of
  RAnd b _ -> Prelude.not b
  ROr b _ -> Prelude.not b
  RNot mb _ -> Prelude.not $ fromMaybe False mb
  RNeverTrue nt _ -> nt == NTFalse
  RNeverFalse nf _ -> False
  ROperand mb -> Prelude.not $ fromMaybe False mb

isDiscard :: RNode -> Bool
isDiscard n = case n of
  RAnd _ _ -> False
  ROr _ _ -> False
  RNot x _ -> isNothing x
  RNeverTrue nt _ -> nt == NTDiscard
  RNeverFalse nf _ -> nf == NFDiscard
  ROperand x -> isNothing x

toMaybeBool :: RNode -> Maybe Bool
toMaybeBool n
  | isTrue n = Just True
  | isFalse n = Just False
  | otherwise = Nothing

evaluate :: a -> Pdct a -> Result
evaluate a (Pdct l n) = Result l n'
  where
    n' = evaluateNode a n

evaluateNode
  :: a
  -> Node a
  -> RNode
evaluateNode a n = case n of
  And ps -> let r = evalAnd a ps in RAnd (fst r) (snd r)
  Or ps -> let r = evalOr a ps in ROr (fst r) (snd r)
  Not p -> let r = evalNot a p in RNot (fst r) (snd r)
  NeverFalse p -> let r = evalNeverFalse a p
                  in RNeverFalse (fst r) (snd r)
  NeverTrue p -> let r = evalNeverTrue a p
                 in RNeverTrue (fst r) (snd r)
  Operand f -> ROperand (f a)

evalAnd :: a -> [Pdct a] -> (Bool, [Result])
evalAnd a ps =
  let rs = map (evaluate a) ps
      f (Result _ n) = case n of
        RAnd b _ -> b
        ROr b _ -> b
        RNot mb _ -> fromMaybe True mb
        RNeverTrue nt _ -> nt == NTDiscard
        RNeverFalse _ _ -> True
        ROperand mb -> fromMaybe True mb
  in (all f rs, rs)

evalOr :: a -> [Pdct a] -> (Bool, [Result])
evalOr a ps =
  let rs = map (evaluate a) ps
      f (Result _ n) = case n of
        RAnd b _ -> b
        ROr b _ -> b
        RNot mb _ -> fromMaybe False mb
        RNeverTrue _ _ -> False
        RNeverFalse nf _ -> nf == NFTrue
        ROperand mb -> fromMaybe False mb
  in (any f rs, rs)


evalNot :: a -> Pdct a -> (Maybe Bool, Result)
evalNot a p =
  let r@(Result _ n) = evaluate a p
      mb = case n of
        RAnd b _ -> Just . Prelude.not $ b
        ROr b _ -> Just . Prelude.not $ b
        RNot m _ -> fmap Prelude.not m
        RNeverTrue nt _ -> case nt of
          NTDiscard -> Nothing
          NTFalse -> Just True
        RNeverFalse nf _ -> case nf of
          NFDiscard -> Nothing
          NFTrue -> Just False
        ROperand m -> fmap Prelude.not m
  in (mb, r)

evalNeverFalse :: a -> Pdct a -> (NeverFalse, Result)
evalNeverFalse a p =
  let r@(Result _ n) = evaluate a p
      mb = case n of
        RAnd b _ -> if b then NFTrue else NFDiscard
        ROr b _ -> if b then NFTrue else NFDiscard
        RNot m _ -> maybe NFDiscard
                    (\b -> if b then NFTrue else NFDiscard) m
        RNeverTrue _ _ -> NFDiscard
        RNeverFalse x _ -> x
        ROperand m -> maybe NFDiscard
                        (\b -> if b then NFTrue else NFDiscard) m
  in (mb, r)

evalNeverTrue :: a -> Pdct a -> (NeverTrue, Result)
evalNeverTrue a p =
  let r@(Result _ n) = evaluate a p
      fromB b = if b then NTDiscard else NTFalse
      mb = case n of
        RAnd b _ -> fromB b
        ROr b _ -> fromB b
        RNot m _ -> maybe NTDiscard fromB m
        RNeverTrue x _ -> x
        RNeverFalse _ _ -> NTDiscard
        ROperand m -> maybe NTDiscard fromB m
  in (mb, r)

evalOperand :: a -> (a -> Maybe Bool) -> Maybe Bool
evalOperand = flip ($)

showResult
  :: IndentAmt
  -> ShowDiscards
  -> Level
  -> Result
  -> [R.Chunk]
showResult i d lvl (Result lbl n)
  | isDiscard n && Prelude.not d = []
  | otherwise = indent i lvl chunks ++ rest
  where
    chunks = labelBool lbl (toMaybeBool n)
    rest = case n of
      RAnd _ rs -> concatMap (showResult i d (lvl + 1)) rs
      ROr _ rs -> concatMap (showResult i d (lvl + 1)) rs
      RNot _ r -> showResult i d (lvl + 1) r
      RNeverTrue _ r -> showResult i d (lvl + 1) r
      RNeverFalse _ r -> showResult i d (lvl + 1) r
      ROperand _ -> []


showTopResult
  :: X.Text
  -> IndentAmt
  -> ShowDiscards
  -> Result
  -> [R.Chunk]
showTopResult txt i sd r = showResult i sd 0 r'
  where
    r' = r { rLabel = rLabel r <> " - " <> txt }

