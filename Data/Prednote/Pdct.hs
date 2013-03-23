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
  , eval
  , evaluate
  ) where

import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat, mempty)
import qualified System.Console.Rainbow as R
import System.Console.Rainbow ((+.+))
import Prelude hiding (not, and, or)
import qualified Prelude

type Label = Text

-- | A tree of predicates.
data Pdct a = Pdct Label (Node a)

instance Show (Pdct a) where
  show _ = "predicate"

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
-- Just True. Otherwise, the Pdct returns Nothing.  Has no effect on
-- non-Operand Pdct.
neverFalse :: Pdct a -> Pdct a
neverFalse = Pdct "never False" . NeverFalse

-- | Turns an existing Pdct to one that never says True. If the
-- underlying predicate returns Just False, the new Pdct also returns
-- Just False. Otherwise, the Pdct returns Nothing.  Has no effect on
-- non-Operand Pdct.
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
    idt = R.plain (X.replicate (lvl * amt) " ")
    nl = R.plain (X.singleton '\n')

-- | Shows a Pdct tree without evaluating it.
showPdct :: IndentAmt -> Level -> Pdct a -> [R.Chunk]
showPdct amt lvl (Pdct l pd) = case pd of
  And ls -> indent amt lvl [R.plain l]
            <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Or ls -> indent amt lvl [R.plain l]
           <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Not t -> indent amt lvl [R.plain l]
           <> showPdct amt (lvl + 1) t
  NeverFalse t -> indent amt lvl [R.plain l]
                  <> showPdct amt (lvl + 1) t
  NeverTrue t -> indent amt lvl [R.plain l]
                 <> showPdct amt (lvl + 1) t
  Operand _ -> indent amt lvl [R.plain l]


labelBool :: Text -> Maybe Bool -> [R.Chunk]
labelBool t b = [open, trueFalse, close, blank, txt]
  where
    trueFalse = case b of
      Nothing -> R.plain "discard" +.+ R.f_yellow
      Just bl -> if bl
        then R.plain "TRUE" +.+ R.f_green
        else R.plain "FALSE" +.+ R.f_red
    open = R.plain "["
    close = R.plain "]"
    blank = R.plain (X.replicate blankLen " ")
    blankLen = X.length "discard"
               - X.length (R.chunkText trueFalse) + 1
    txt = R.plain t

type ShowDiscards = Bool

-- | Evaluates a Pdct.
eval :: Pdct a -> a -> Maybe Bool
eval (Pdct _ n) a = case n of
  And ps -> Just . Prelude.and . catMaybes $ [flip eval a] <*> ps
  Or ps -> Just . Prelude.or . catMaybes $ [flip eval a] <*> ps
  Not p -> fmap Prelude.not $ eval p a
  NeverFalse p -> case eval p a of
    Nothing -> Nothing
    Just b -> if Prelude.not b then Nothing else Just b
  NeverTrue p -> case eval p a of
    Nothing -> Nothing
    Just b -> if b then Nothing else Just b
  Operand f -> f a

-- | Verbosely evaluates a Pdct.
evaluate
  :: IndentAmt
  -- ^ Indent each level by this many spaces.

  -> ShowDiscards
  -- ^ If True, show discarded test results; otherwise, hide
  -- them.

  -> a
  -- ^ The subject to evaluate

  -> Level
  -- ^ How many levels deep in the tree we are. Typically you will
  -- start at level 0. This determines the level of indentation.

  -> Pdct a
  -> (Maybe Bool, [R.Chunk])
evaluate i sd a lvl (Pdct l pd) = case pd of

  And ps -> let (resBool, resTxt) = evalAnd i sd a (lvl + 1) ps
                txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
            in (Just resBool, txt)

  Or ps -> let (resBool, resTxt) = evalOr i sd a (lvl + 1) ps
               txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
           in (Just resBool, txt)

  Not p -> let (childMayBool, childTxt) = evaluate i sd a (lvl + 1) p
               thisMayBool = fmap Prelude.not childMayBool
               thisTxt = indent i lvl (labelBool l thisMayBool)
               txt = if sd || isJust thisMayBool
                     then thisTxt <> childTxt else mempty
           in (thisMayBool, txt)

  NeverFalse p ->
    let (childMayBool, childTxt) = evaluate i sd a (lvl + 1) p
        thisMayBool = case childMayBool of
          Nothing -> Nothing
          Just b -> if Prelude.not b then Nothing else Just b
        thisTxt = indent i lvl (labelBool l thisMayBool)
        txt = if sd || isJust thisMayBool
              then thisTxt <> childTxt else mempty
    in (thisMayBool, txt)

  NeverTrue p ->
    let (childMayBool, childTxt) = evaluate i sd a (lvl + 1) p
        thisMayBool = case childMayBool of
          Nothing -> Nothing
          Just b -> if b then Nothing else Just b
        thisTxt = indent i lvl (labelBool l thisMayBool)
        txt = if sd || isJust thisMayBool
              then thisTxt <> childTxt else mempty
    in (thisMayBool, txt)

  Operand p -> let res = p a
                   txt = indent i lvl (labelBool l res)
               in (res, if sd || isJust res then txt else mempty)

evalAnd :: IndentAmt -> ShowDiscards -> a
        -> Level -> [Pdct a] -> (Bool, [R.Chunk])
evalAnd i sd a l ts = (Prelude.not foundFalse, txt)
  where
    (foundFalse, txt) = go ts (False, mempty)
    go [] p = p
    go (x:xs) (fndFalse, acc) =
      if fndFalse
      then (fndFalse, acc <> indent i l
                             [R.plain "(short circuit)"])
      else let (res, cTxt) = evaluate i sd a l x
               fndFalse' = maybe False Prelude.not res
           in go xs (fndFalse', acc <> cTxt)

evalOr :: IndentAmt -> ShowDiscards -> a
       -> Level -> [Pdct a] -> (Bool, [R.Chunk])
evalOr i sd a l ts = (foundTrue, txt)
  where
    (foundTrue, txt) = go ts (False, mempty)
    go [] p = p
    go (x:xs) (fnd, acc) =
      if fnd
      then (fnd, acc <> indent i l
                        [R.plain "(short circuit)"])
      else let (res, cTxt) = evaluate i sd a l x
               fnd' = fromMaybe False res
           in go xs (fnd', acc <> cTxt)

