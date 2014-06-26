{-# LANGUAGE ExistentialQuantification #-}

-- | 'Pred' core functions.  If your needs are simple, "Prednote.Pred"
-- is easier to use.  However, the types and functions in this module
-- give you more control.
module Prednote.Pred.Core
  (
  -- * Data types
    Tree(..)
  , Pred(..)
  , Output(..)
  , Shortable(..)
  , Calc(..)

  -- * Functions
  , display
  , evaluate
  , render
  , test
  , filter
  , testV
  , filterV
  ) where

import System.Console.Rainbow
import Prelude hiding (filter)
import qualified Prelude
import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.Text as X
import Data.List (intersperse)

-- | A rose tree.
data Tree a = Tree
  { node :: a
  , children :: [Tree a]
  } deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap f (Tree n cs) = Tree (f n) (map (fmap f) cs)

-- | A predicate.  This type holds a /static label/, which allows the
-- 'Pred' to be shown even when it is not being applied to a value.
-- Think of the 'Pred' as being a tree of predicates; either it is a
-- leaf (which is a predicate) or a branch (which has one or more
-- predicates as children.)
--
-- The 'Pred' is parameterized on the type that it tests against; for
-- example, a 'Pred' 'Int' is a predicate on 'Int'.
data Pred a = Pred
  { label :: Int -> [Chunk]
  -- ^ A /static label/, used to show the predicate when using
  -- 'display' or the 'Show' instance.  This allows you to display a
  -- 'Pred' even when it is not being applied to a value.

  , calc :: Calc a
  -- ^ The payload of the 'Pred'.
  }

instance Contravariant Pred where
  contramap f (Pred l c) = Pred l (contramap f c)

-- | The output from a predicate function.
data Output = Output
  { visible :: Bool
  -- ^ Is this result visible?  Results that are not visible are not
  -- shown when using functions such as 'evaluate', 'testV', or
  -- 'filterV'.  In addition, none of the children of hidden results
  -- are shown.  Invisible results still affect the 'Pred' that
  -- contain them; they simply are not shown.

  , result :: Bool
  -- ^ Whether the predicate evaluates to 'True' or 'False'.

  , report :: Int -> [Chunk]
  -- ^ A report describing the output.  This function is applied to
  -- the depth of the 'Pdct' in the tree; you can use this to perform
  -- indentation.  The function should return a single line of text,
  -- ending with a newline.  If you want the report to indicate
  -- whether the result is 'True' or 'False', put that in the
  -- 'Chunk's; you might indicate this with a text string, or by using
  -- color, or both.
  }

instance Show Output where
  show (Output v r _) = "output - visible: " ++ show v
    ++ " result: " ++ show r

-- | A result returned from an evaluation that might /short circuit/.
-- Short circuiting may occur with 'Variable' or 'Fan' calculations.
-- A short circuit occurs when the calculation need not consider all
-- the results of the child calculations.  For example, for a function
-- like 'Prelude.all' in the Prelude, evaluation stops immediately
-- when a 'False' list element is encountered; if such an element is
-- encountered before the end of a list, that's an example of a short
-- circuit.
data Shortable = Shortable
  { short :: Maybe (Int, Int -> [Chunk])
  -- ^ If the computation short circuited, return a 'Just' here.  The
  -- first element of the pair is the number of 'Bool's needed to
  -- arrive at a result.  The second element of the pair is a function
  -- that, when applied to the level of nesting of the 'Pdct', returns
  -- the text to show to indicate that a short circuit occurred.
  , output :: Output
  }

instance Show Shortable where
  show (Shortable m o) = "shortable - short is: " ++ sm ++ " "
    ++ show o
    where
      sm = maybe "Nothing" (const "Just") m

data Calc a
  = Predicate (a -> Output)
  -- ^ A predicate; has no children.  The function is applied to the
  -- subject of the test.

  | Single (Pred a) (a -> Bool -> Output)
  -- ^ A 'Pred' that has a single child 'Pred'.  The data
  -- constructor's second argument is a function that is applied to
  -- the subject and to the result of the application of the child
  -- 'Pred' to that subject; that function returns an 'Output'.

  | Variable [Pred a] (a -> [Bool] -> Shortable)
  -- ^ A 'Pred' that has zero or more child 'Pred'.  The data
  -- constructor's first argument is the list of child 'Pred'.  The
  -- second argument is a function that is applied to the subject and
  -- to the result of the application of the child 'Pred' to the
  -- subject; that function returns a 'Shortable'.

  | forall b. Fan (a -> [b]) (Pred b) (a -> [Bool] -> Shortable)
  -- ^ A 'Pred' that has a single child 'Pred' that is applied to the
  -- result of splitting the subject into multiple items.  The data
  -- constructor's first argument is a function that splits the
  -- subject into multiple items.  This is called the /splitter/.  The
  -- second argument is the child 'Pred'.  The third argument is a
  -- function that is applied to the subject and to the result of the
  -- the application of the child 'Pred' to each result from the
  -- splitter; this function returns a 'Shortable'.

instance Show (Calc a) where
  show c = case c of
    Predicate _ -> "predicate"
    Single p _ -> "single: " ++ show p
    Variable ps _ -> "variable: " ++ concat (intersperse " - " .
      map show $ ps)
    Fan _ p _ -> "fan: " ++ show p

instance Contravariant Calc where
  contramap f c = case c of
    Predicate g -> Predicate (g . f)
    Single g h -> Single (contramap f g) (\a b -> h (f a) b)
    Variable ps g -> Variable (map (contramap f) ps) (\a b -> g (f a) b)
    Fan g p h -> Fan (g . f) p (\a b -> h (f a) b)


-- | Display a 'Pred' without evaluating it.
display
  :: Int
  -- ^ Indentation level.  Ordinarily you will start at 0.
  -> Pred a
  -> [Chunk]
display lvl (Pred lbl c) = lbl lvl ++ case c of
  Predicate _ -> []
  Single p _ -> display (lvl + 1) p
  Variable ps _ -> concatMap (display (lvl + 1)) ps
  Fan _ p _ -> display (lvl + 1) p

instance Show (Pred a) where
  show = X.unpack . X.concat
    . map (X.concat . text)
    . display 0

evaluate
  :: Pred a
  -- ^ 'Pred' to evaluate

  -> a
  -- ^ The subject

  -> Tree (Maybe (Int -> [Chunk]), Output)
  -- ^ 'Tree' of results.  Each 'node' is a pair, with the first
  -- element indicating whether the result has a short circuit.  It is
  -- 'Nothing' if the result is not a short circuit, or 'Just' with a
  -- function that, when applied to the depth level, returns an
  -- indication of the short circuit.
  --
  -- The second element of the pair is the 'Output' with the result
  -- and the report.

evaluate (Pred _ clc) a = case clc of
  Predicate f -> Tree (Nothing, f a) []
  Single p f ->
    let c = evaluate p a
        cRes = result . snd . node $ c
        o = f a cRes
    in Tree (Nothing, o) [c]

  Variable ps f ->
    let cs = map (flip evaluate a) ps
        bs = map (result . snd . node) cs
        shortable = f a bs
        (cs', maySS) = case short shortable of
          Nothing -> (cs, Nothing)
          Just (len, ss) -> (take len cs, Just ss)
        ssOut
          | cs' `shorter` cs = maySS
          | otherwise = Nothing
    in Tree (ssOut, output shortable) cs'

  Fan fanner p f ->
    let ps = fanner a
        cs = map (evaluate p) ps
        bs = map (result . snd . node) cs
        shortable = f a bs
        (cs', maySS) = case short shortable of
          Nothing -> (cs, Nothing)
          Just (len, ss) -> (take len cs, Just ss)
        ssOut
          | cs' `shorter` cs = maySS
          | otherwise = Nothing
    in Tree (ssOut, output shortable) cs'

-- | Evaluate a 'Pred' against a single subject, returning only the
-- 'result'.
test :: Pred a -> a -> Bool
test p = result . snd . node . evaluate p

-- | Like 'Prelude.filter' but using a 'Pred'.
filter :: Pred a -> [a] -> [a]
filter p = Prelude.filter (test p)

-- | Like 'test' but verbose: returns a description of the evaluation
-- process as well as the 'result'.
testV
  :: Int
  -- ^ Indentation level.  Ordinarily you will start this off at zero.
  -> Pred a
  -> a
  -- ^ The subject.
  -> ([Chunk], Bool)
testV idt p a = (render idt t, result . snd . node $ t)
  where
    t = evaluate p a

-- | Like 'filter' but verbose: returns a description of the
-- evaluation process as well as the list of elements for which the
-- 'Pred' returned 'True'.
filterV
  :: Int
  -- ^ Indentation level.  Ordinarily you will start this off at zero.

  -> Pred a

  -> [a]
  -- ^ List of items to filter

  -> ([Chunk], [a])

filterV idt p as = (chks, rslts)
  where
    ts = map (evaluate p) as
    chks = concatMap (render idt) ts
    rslts
      = map fst
      . Prelude.filter (result . snd . node . snd)
      $ zip as ts

-- | Renders a tree of results.
render
  :: Int
  -- ^ Indentation level.  Ordinarily you will start this off at zero.

  -> Tree (Maybe (Int -> [Chunk]), Output)
  -- ^ Results to render; for example, 'evaluate' returns a 'Tree' of
  -- this type.

  -> [Chunk]

render l (Tree n cs)
  | Prelude.not . visible . snd $ n = []
  | otherwise = lbl ++ kids ++ shrt
  where
    lbl = (report . snd $ n) l
    kids = concatMap (render (l + 1)) cs
    shrt = case fst n of
      Nothing -> []
      Just f -> f (l + 1)

-- | @shorter x y@ is True if list x is shorter than list y. Lazier
-- than taking the length of each list and comparing the results.
shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

