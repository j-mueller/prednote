{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

-- | Trees of predicates.
--
-- Exports names which conflict with Prelude names, so you probably
-- want to import this module qualified.

module Prednote.Predbox.Core
  ( -- * The Predbox tree
    Predbox(..)
  , Labels(..)
  , sameLabel
  , Node(..)

  -- * Creating Predbox.
  -- | All functions create Predbox that are shown by default.
  , predicate
  , and
  , or
  , not
  , (&&&)
  , (|||)

  -- * Controlling whether Predbox are shown in the results
  , hide
  , visible
  , hideTrue
  , hideFalse

  -- * Result
  , Result(..)
  , RNode(..)
  , showAll

  -- * Showing and evaluating Predbox
  , evaluate
  , evaluateNode
  , Format(..)
  , showResult
  , showPredbox
  , filter
  , verboseFilter

  ) where


-- # Imports

import Data.Functor.Contravariant hiding (Predicate)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat)
import Data.String (fromString)
import System.Console.Rainbow
import Prelude hiding (not, and, or, compare, filter, show)
import qualified Prelude

-- # Predbox type

-- | A predicate. Each Predbox contains a tree of Node.
data Predbox a = Predbox
  { pVisible :: (Bool -> Bool)
  -- ^ As results are computed, this function is applied to the
  -- result. If this function returns False, then this Predbox will not
  -- be shown by default in the results.

  , pNode :: Node a

  }

data Labels a = Labels
  { static :: [Chunk]
  , runtime :: a -> [Chunk]
  }

instance Contravariant Labels where
  contramap g (Labels s r) = Labels s (r . g)

sameLabel :: [Chunk] -> Labels a
sameLabel x = Labels x (const x)

data Node a
  = And [Predbox a]
  -- ^ Conjunction. If any Predbox in the list is False, the result is
  -- False. If the list is empty, the result is True.

  | Or [Predbox a]
  -- ^ Disjunction. If at least one Predbox in the list is True, the
  -- result it True. If the list is empty, the result is False.

  | Not (Predbox a)
  -- ^ Negation

  | forall b. Fanand (Labels a) (a -> [b]) (Predbox b)
  -- ^ Fanout conjunction.  The given function is used to derive a
  -- list of subjects from a single subject; then, the given 'Predbox'
  -- is applied to each subject.  Each subject must be 'True' for the
  -- resulting predicate to be 'True'.

  | forall b. Fanor (Labels a) (a -> [b]) (Predbox b)
  -- ^ Fanout disjunction.  The given function is used to derive a
  -- list of subjects from a single subject; then, the given 'Predbox'
  -- is applied to each subject.  At least one subject must be 'True'
  -- for the resulting predicate to be 'True'.

  | Predicate (Labels a) (a -> Bool)
  -- ^ Most basic building block.

-- | Creates and labels predicates.
predicate :: [Chunk] -> (a -> [Chunk]) -> (a -> Bool) -> Predbox a
predicate st dy pd = Predbox (const True) $
  Predicate (Labels st dy) pd

-- | Creates And Predbox
and :: [Predbox a] -> Predbox a
and = Predbox (const True) . And

-- | Creates Or Predbox
or :: [Predbox a] -> Predbox a
or = Predbox (const True) . Or

-- | Creates Not Predbox
not :: Predbox a -> Predbox a
not = Predbox (const True) . Not

-- | Changes a Predbox so it is always hidden by default.
hide :: Predbox a -> Predbox a
hide p = p { pVisible = const False }

-- | Changes a Predbox so it is always shown by default.
visible :: Predbox a -> Predbox a
visible p = p { pVisible = const True }

-- | Changes a Predbox so that it is hidden if its result is True.
hideTrue :: Predbox a -> Predbox a
hideTrue p = p { pVisible = Prelude.not }

-- | Changes a Predbox so that it is hidden if its result is False.
hideFalse :: Predbox a -> Predbox a
hideFalse p = p { pVisible = id }

-- | Forms a Predbox using 'and'.
(&&&) :: Predbox a -> Predbox a -> Predbox a
(&&&) x y = Predbox (const True) (And [x, y])
infixr 3 &&&

-- | Forms a Predbox using 'or'.
(|||) :: Predbox a -> Predbox a -> Predbox a
(|||) x y = Predbox (const True) (Or [x, y])
infixr 2 |||

instance Contravariant Predbox where
  contramap f (Predbox l n) = Predbox l $ contramap f n

instance Contravariant Node where
  contramap f n = case n of
    And ls -> And $ map (contramap f) ls
    Or ls -> Or $ map (contramap f) ls
    Not o -> Not $ contramap f o
    Fanand l g b -> Fanand (contramap f l) (g . f) b
    Fanor l g b -> Fanor (contramap f l) (g . f) b
    Predicate l g -> Predicate (contramap f l) (g . f)

-- # Result

-- | The result from evaluating a Predbox.
data Result = Result
  { rBool :: Bool
  -- ^ The boolean result from evaluating the node. If the node is an
  -- predicate, this is the result of applying the predicate function to
  -- the subject. Otherwise, this is the result of application of the
  -- appropriate boolean operation to the child nodes.

  , rVisible :: Bool
  -- ^ Is this result shown? Hiding only
  -- affects presentation; it does not affect how this Predbox affects
  -- any parent Predbox.
  , rNode :: RNode
  } deriving (Eq, Show)

data RNode
  = RAnd [Result]
  | ROr [Result]
  | RNot Result
  | RFanand [Chunk] [Result]
  | RFanor [Chunk] [Result]
  | RPredicate [Chunk] Bool
  deriving (Eq, Show)

-- | Changes all items in a Result so they are shown.
showAll :: Result -> Result
showAll (Result r _ n) = Result r True n'
  where
    n' = case n of
      RAnd rs -> RAnd (map showAll rs)
      ROr rs -> ROr (map showAll rs)
      RNot x -> RNot (showAll x)
      RFanand cs rs -> RFanand cs (map showAll rs)
      RFanor cs rs -> RFanor cs (map showAll rs)
      x -> x

-- | Applies a Predbox to a particular value, known as the subject.
evaluate :: Predbox a -> a -> Result
evaluate (Predbox pv pn) a = Result r (pv r) rn
  where
    rn = evaluateNode pn a
    r = case rn of
      RAnd ls -> all rBool ls
      ROr ls -> any rBool ls
      RNot x -> Prelude.not . rBool $ x
      RFanand _ ls -> all rBool ls
      RFanor _ ls -> any rBool ls
      RPredicate _ b -> b

evaluateNode :: Node a -> a -> RNode
evaluateNode n a = case n of
  And ls -> RAnd (map (flip evaluate a) ls)
  Or ls -> ROr (map (flip evaluate a) ls)
  Not l -> RNot (flip evaluate a l)
  Fanand (Labels _ rt) f b -> RFanand (rt a) (map (evaluate b) (f a))
  Fanor (Labels _ rt) f b -> RFanor (rt a) (map (evaluate b) (f a))
  Predicate (Labels _ rt) f -> RPredicate (rt a) (f a)

-- # Types and functions for showing

-- | Indents text, and adds a newline to the end.
indent
  :: Int
  -- ^ Indent each level by this number of spaces
  -> Int
  -- ^ Number of levels
  -> [Chunk]
  -> [Chunk]
indent amt lvl cs = idt : (cs ++ [nl])
  where
    idt = fromString (replicate (lvl * amt) ' ')
    nl = fromString "\n"

-- # Showing Predbox

-- | Shows a Predbox tree without evaluating it.
showPredbox
  :: [Chunk]
  -- ^ How to show @and@
  -> [Chunk]
  -- ^ How to show @or@
  -> [Chunk]
  -- ^ How to show @not@
  -> Int
  -- ^ Indent each level by this amount
  -> Int
  -- ^ Current level
  -> Predbox a
  -> [Chunk]
showPredbox cAnd cOr cNot amt lvl (Predbox _ pd) = case pd of
  And ls -> indent amt lvl cAnd
            <> mconcat (map (showPredbox cAnd cOr cNot amt (lvl + 1)) ls)
  Or ls -> indent amt lvl cOr
           <> mconcat (map (showPredbox cAnd cOr cNot amt (lvl + 1)) ls)
  Not t -> indent amt lvl cNot
           <> showPredbox cAnd cOr cNot amt (lvl + 1) t
  Fanand (Labels l _) _ p -> indent amt lvl l
    <> showPredbox cAnd cOr cNot amt (lvl + 1) p
  Fanor (Labels l _) _ p -> indent amt lvl l
    <> showPredbox cAnd cOr cNot amt (lvl + 1) p
  Predicate (Labels l _) _ -> indent amt lvl l

instance Show (Predbox a) where
  show = X.unpack
       . X.concat
       . concat
       . map text
       . showPredbox [fromText "and"] [fromText "or"]
          [fromText "not"] 2 0


filter :: Predbox a -> [a] -> [a]
filter pd as
  = map fst
  . Prelude.filter (rBool . snd)
  . zip as
  . map (evaluate pd)
  $ as


-- # Showing Result

data Format = Format
  { fResult :: Bool -> Int -> [Chunk] -> [Chunk] -> [Chunk]
  -- ^ Formats results.  This function is applied to the result
  -- itself, to the level of indentation, to any pre-result label, and
  -- to the rest of the text on the line; it should return a properly
  -- indented line along with the indication of truth or falsity.
  -- Include a single newline.

  , fAnd :: [Chunk]
  -- ^ Indicates conjunction.  The result from this will be passed to
  -- 'fTrue' or 'fFalse' as appropriate.

  , fOr :: [Chunk]
  -- ^ Indicates disjunction.  The result from this will be passed to
  -- 'fTrue' or 'fFalse' as appropriate.

  , fNot :: [Chunk]
  -- ^ Indicates negation.  The result from this will be passed to
  -- 'fTrue' or 'fFalse' as appropriate.

  , fShort :: Int -> [Chunk]
  -- ^ Indicates a short-circuit.  This happens when 'And', 'Or',
  -- 'Fanand', or 'Fanor' do not need to evaluate the entire list.
  -- This function will be applied to the level of indentation.
  }

-- | Shows a Result in a pretty way with colors and indentation.
showResult
  :: Format

  -> [Chunk]
  -- ^ Additional label

  -> Int
  -- ^ How deep in the tree we are; this increments by one for each
  -- level of descent.

  -> Result
  -- ^ The result to show

  -> [Chunk]
showResult fmt addl lvl (Result rslt vis nd)
  | Prelude.not vis = []
  | otherwise = firstLine ++ restLines
  where
    showMore = showResult fmt []
    firstLine = fResult fmt rslt lvl addl lbl
    (lbl, restLines) = case nd of
      RAnd ls -> (fAnd fmt, f False ls)
      ROr ls -> (fOr fmt, f True ls)
      RNot r -> (fNot fmt, showMore (lvl + 1) r)
      RFanand l ls -> (l, f False ls)
      RFanor l ls -> (l, f True ls)
      RPredicate l _ -> (l, [])
    f stopOn ls = concatMap (showMore (lvl + 1)) ls' ++ end
      where
        ls' = takeThrough ((== stopOn) . rBool) ls
        end = if ls' `shorter` ls
              then fShort fmt (lvl + 1)
              else []

-- | @shorter x y@ is True if list x is shorter than list y. Lazier
-- than taking the length of each list and comparing the results.
shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

-- | For instance,
--
-- > takeThrough odd [2,4,6,7,8] == [2,4,6,7]
takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough f (x:xs) = x : if f x then [] else takeThrough f xs


-- | Filters a list. Also returns chunks describing the process.
verboseFilter
  :: Format
  -> Bool
  -- ^ If True, shows all Predbox, even ones where 'rHide' is
  -- True. Otherwise, respects 'rHide' and does not show hidden Predbox.

  -> (a -> [Chunk])
  -- ^ How to describe each subject

  -> Predbox a
  -- ^ Used to perform the filtering

  -> [a]
  -> ([Chunk], [a])

verboseFilter fmt sa desc pd as = (chks, as')
  where
    rs = map (evaluate pd) as
    subjAndRslts = zip as rs
    mkChks (subj, rslt) = showResult fmt (desc subj) 0 rslt'
      where
        rslt' | sa = showAll rslt
              | otherwise = rslt
    chks = concatMap mkChks subjAndRslts
    as' = map fst . Prelude.filter (rBool . snd) $ subjAndRslts

