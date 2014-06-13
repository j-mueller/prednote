{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

-- | Trees of predicates.
--
-- Exports names which conflict with Prelude names, so you probably
-- want to import this module qualified.

module Prednote.Predbox

  ( -- * The Predbox tree
    Label
  , Visible
  , Predbox(..)
  , Node(..)

  -- * Creating Predbox.
  -- | All functions create Predbox that are shown by default.
  , predicate
  , and
  , or
  , not
  , fanand
  , fanor
  , (&&&)
  , (|||)
  , always
  , never

  -- * Controlling whether Predbox are shown in the results
  , hide
  , visible
  , hideTrue
  , hideFalse

  -- * Renaming Predbox
  , rename

  -- * Result
  , Result(..)
  , RNode(..)

  -- * Showing and evaluating Predbox
  , evaluate
  , evaluateNode
  , IndentAmt
  , Level
  , ShowAll
  , showResult
  , showTopResult
  , showPredbox
  , filter
  , verboseFilter

  -- * Helpers for building common Predbox
  -- ** Non-overloaded

  -- | Each of these functions builds a Predbox that compares two
  -- items.  The predicate in the Predbox is applied to an item that
  -- is considered to be the left hand side of the comparison.  The
  -- left hand side side can change; the right hand side is baked
  -- into the Predbox.
  --
  -- For example, to build a Predbox that returns True if an item is
  -- greater than 5:
  --
  -- >>> :set -XOverloadedStrings
  -- >>> let p = compareBy "5" "integer" (`Prelude.compare` (5 :: Integer)) GT
  -- >>> rBool . evaluate p $ 6
  -- True
  -- >>> rBool . evaluate p $ 4
  -- False
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


-- # Imports

import Data.Functor.Contravariant hiding (Predicate)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat, mempty)
import Data.String (fromString)
import qualified System.Console.Rainbow as R
import Prelude hiding (not, and, or, compare, filter, show)
import qualified Prelude

-- # Predbox type

-- | A predicate. Each Predbox contains a tree of Node.
data Predbox a = Predbox
  { pVisible :: (Bool -> Text)
  -- ^ As results are computed, this function is applied to the
  -- result. If this function returns False, then this Predbox will not
  -- be shown by default in the results.

  , pNode :: Node a

  }

data Labels a = Labels
  { static :: Text
  , runtime :: a -> Text
  }

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

-- | Always True
always :: Predbox a
always = Predbox (const True) (Predicate lbl (const True))
  where
    lbl = Labels txt (const txt)
    txt = "always True"

-- | Always False
never :: Predbox a
never = Predbox (const True) (Predicate lbl (const False))
  where
    lbl = Labels txt (const txt)
    txt = "always False"

-- | Creates and labels predicates.
predicate :: Text -> (a -> Text) -> (a -> Bool) -> Predbox a
predicate st dy pd = Predbox (const True) $
  Predicate (Labels st dy) pd

-- | Creates And Predbox using a generic name
and :: [Predbox a] -> Predbox a
and = Predbox (const True) . And

-- | Creates Or Predbox using a generic name
or :: [Predbox a] -> Predbox a
or = Predbox (const True) . Or

-- | Creates Not Predbox using a generic name
not :: Predbox a -> Predbox a
not = Predbox (const True) . Not

-- | Creates a 'Fanand' Predbox using a generic name.
fanand
  :: Text
  -- ^ Label to use for static tree

  -> (a -> Text)
  -- ^ Label to use when when using tree

  -> (a -> [b])
  -- ^ This function is applied to every subject to derive a list of
  -- new subjects.

  -> Predbox b
  -- ^ This Predbox is applied to each of the new subjects.  The
  -- resulting predicate is 'True' if each of the new subjects is also
  -- True.

  -> Predbox a
fanand st dy f = Predbox  (const True) $ Fanand (Labels st dy) f

-- | Creates a 'Fanor' Predbox using a generic name.
fanor
  :: Text
  -- ^ Label to use for static tree

  -> (a -> Text)
  -- ^ Label to use when when using tree

  -> (a -> [b])
  -- ^ This function is applied to every subject to derive a list of
  -- new subjects.

  -> Predbox b
  -- ^ This Predbox is applied to each of the new subjects.  The
  -- resulting predicate is 'True' if one of the new subjects is also
  -- True.

  -> Predbox a
fanor st dy f = Predbox "or" (const True) $ Fanor (Labels st dy) f

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

-- | Forms a Predbox using 'and'; assigns a generic label.
(&&&) :: Predbox a -> Predbox a -> Predbox a
(&&&) x y = Predbox (const True) (And [x, y])
infixr 3 &&&

-- | Forms a Predbox using 'or'; assigns a generic label.
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
    Fanand g b -> Fanand (g . f) b
    Fanor g b -> Fanor (g . f) b
    Predicate g -> Predicate $ \b -> g (f b)

-- # Result

-- | The result from evaluating a Predbox.
data Result = Result
  { rLabel :: Label
  -- ^ The label from the original Predbox

  , rBool :: Bool
  -- ^ The boolean result from evaluating the node. If the node is an
  -- predicate, this is the result of applying the predicate function to
  -- the subject. Otherwise, this is the result of application of the
  -- appropriate boolean operation to the child nodes.

  , rVisible :: Visible
  -- ^ Is this result shown? Hiding only
  -- affects presentation; it does not affect how this Predbox affects
  -- any parent Predbox.
  , rNode :: RNode
  } deriving (Eq, Show)

data RNode
  = RAnd [Result]
  | ROr [Result]
  | RNot Result
  | RFanand [Result]
  | RFanor [Result]
  | RPredicate Bool
  deriving (Eq, Show)

-- | Applies a Predbox to a particular value, known as the subject.
evaluate :: Predbox a -> a -> Result
evaluate (Predbox l d n) a = Result l r d' rn
  where
    rn = evaluateNode n a
    r = case rn of
      RAnd ls -> all rBool ls
      ROr ls -> any rBool ls
      RNot x -> Prelude.not . rBool $ x
      RFanand ls -> all rBool ls
      RFanor ls -> any rBool ls
      RPredicate b -> b
    d' = d r

evaluateNode :: Node a -> a -> RNode
evaluateNode n a = case n of
  And ls -> RAnd (map (flip evaluate a) ls)
  Or ls -> ROr (map (flip evaluate a) ls)
  Not l -> RNot (flip evaluate a l)
  Fanand f b -> RFanand (map (evaluate b) (f a))
  Fanor f b -> RFanor (map (evaluate b) (f a))
  Predicate f -> RPredicate (f a)

-- # Types and functions for showing

-- | The number of spaces to use for each level of indentation.
type IndentAmt = Int

-- | How many levels of indentation to use. Typically you will start
-- this at zero. It is incremented by one for each level as functions
-- descend through the tree.
type Level = Int

-- | Indents text, and adds a newline to the end.
indent :: IndentAmt -> Level -> [R.Chunk] -> [R.Chunk]
indent amt lvl cs = idt : (cs ++ [nl])
  where
    idt = fromString (replicate (lvl * amt) ' ')
    nl = fromString "\n"

-- # Showing Predbox

-- | Creates a plain Chunk from a Text.
plain :: Text -> R.Chunk
plain = R.Chunk mempty . (:[])

-- | Shows a Predbox tree without evaluating it.
showPredbox :: IndentAmt -> Level -> Predbox a -> [R.Chunk]
showPredbox amt lvl (Predbox l _ pd) = case pd of
  And ls -> indent amt lvl [plain ("and - " <> l)]
            <> mconcat (map (showPredbox amt (lvl + 1)) ls)
  Or ls -> indent amt lvl [plain ("or - " <> l)]
           <> mconcat (map (showPredbox amt (lvl + 1)) ls)
  Not t -> indent amt lvl [plain ("not - " <> l)]
           <> showPredbox amt (lvl + 1) t
  Fanand _ p -> indent amt lvl [plain ("and - " <> l)]
    <> showPredbox amt (lvl + 1) p
  Fanor _ p -> indent amt lvl [plain ("or - " <> l)]
    <> showPredbox amt (lvl + 1) p
  Predicate _ -> indent amt lvl [plain ("predicate - " <> l)]

instance Show (Predbox a) where
  show = X.unpack
       . X.concat
       . concat
       . map R.text
       . showPredbox 2 0


filter :: Predbox a -> [a] -> [a]
filter pd as
  = map fst
  . Prelude.filter (rBool . snd)
  . zip as
  . map (evaluate pd)
  $ as


-- # Showing Result

labelBool :: Text -> Bool -> [R.Chunk]
labelBool t b = [open, trueFalse, close, blank, txt]
  where
    trueFalse = 
      if b then "TRUE" <> R.f_green else "FALSE" <> R.f_red
    open = "["
    close = "]"
    blank = plain (X.replicate blankLen " ")
    blankLen = X.length "FALSE"
               - (sum . map X.length . R.text $ trueFalse) + 1
    txt = plain t

type ShowAll = Bool

-- | Shows a Result in a pretty way with colors and indentation.
showResult
  :: IndentAmt
  -- ^ Indent each level by this many spaces

  -> ShowAll
  -- ^ If True, shows all Predbox, even ones where 'rHide' is
  -- True. Otherwise, respects 'rHide' and does not show hidden Predbox.

  -> Level
  -- ^ How deep in the tree we are; this increments by one for each
  -- level of descent.

  -> Result
  -- ^ The result to show

  -> [R.Chunk]
showResult amt sa lvl (Result lbl rslt shw nd)
  | Prelude.not shw && Prelude.not sa = []
  | otherwise = firstLine ++ restLines
  where
    firstLine = indent amt lvl $ labelBool lbl rslt
    restLines = case nd of
      RAnd ls -> f False ls
      ROr ls -> f True ls
      RNot r -> showResult amt sa (lvl + 1) r
      RFanand ls -> f False ls
      RFanor ls -> f True ls
      RPredicate _ -> []
    f stopOn ls = concatMap sr ls' ++ end
      where
        ls' = takeThrough ((== stopOn) . rBool) ls
        sr = showResult amt sa (lvl + 1)
        end = if ls' `shorter` ls
              then indent amt (lvl + 1) ["(short circuit)"]
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

-- | Shows the top of a Result tree and all the child Results. Adds a
-- short label at the top of the tree.
showTopResult
  :: X.Text
  -- ^ Label to add to the top of the tree.
  -> IndentAmt
  -- ^ Indent each level by this many spaces
  -> Level
  -- ^ Indent the top by this many levels
  -> ShowAll
  -- ^ If True, shows all Predbox, even ones where 'rHide' is
  -- True. Otherwise, respects 'rHide' and does not show hidden Predbox.

  -> Result
  -- ^ The result to show
  -> [R.Chunk]
showTopResult txt i lvl sd r = showResult i sd lvl r'
  where
    r' = r { rLabel = rLabel r <> " - " <> txt }


-- | Filters a list. Also returns chunks describing the process.
verboseFilter
  :: (a -> X.Text)
  -- ^ How to describe each subject

  -> IndentAmt
  -- ^ Indent each level by this many spaces

  -> ShowAll
  -- ^ If True, shows all Predbox, even ones where 'rVisible' is
  -- True. Otherwise, respects 'rVisible' and shows only visible
  -- 'Predbox'.

  -> Predbox a
  -- ^ Used to perform the filtering

  -> [a]
  -> ([R.Chunk], [a])

verboseFilter desc amt sa pd as = (chks, as')
  where
    rs = map (evaluate pd) as
    subjAndRslts = zip as rs
    mkChks (subj, rslt) = showTopResult (desc subj) amt 0 sa rslt
    chks = concatMap mkChks subjAndRslts
    as' = map fst . Prelude.filter (rBool . snd) $ subjAndRslts

-- # Comparisons

-- | Build a Predbox that compares items.
compareBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Predbox a

compareBy itemDesc typeDesc cmp ord = Predbox l (const False) (Predicate f)
  where
    l = typeDesc <> " is " <> cmpDesc <> " " <> itemDesc
    cmpDesc = case ord of
      LT -> "less than"
      GT -> "greater than"
      EQ -> "equal to"
    f subj = cmp subj == ord

-- | Overloaded version of 'compareBy'.
compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ The right hand side of the comparison.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Predbox a
compare typeDesc a ord = compareBy itemDesc typeDesc cmp ord
  where
    itemDesc = X.pack . Prelude.show $ a
    cmp item = Prelude.compare item a

-- | Builds a Predbox for items that might fail to return a comparison.
compareByMaybe
  :: Text
  -- ^ How to show the item being compared

  -> Text
  -- ^ Description of type of thing being matched

  -> (a -> Maybe Ordering)
  -- ^ How to compare against right hand side. If Nothing, a Predbox that
  -- always returns False is returned.

  -> Ordering
  -- ^ Ordering that must result for the Predbox to be True

  -> Predbox a

compareByMaybe itemDesc typeDesc cmp ord =
  Predbox l (const False) (Predicate f)
  where
    l = typeDesc <> " is " <> cmpDesc <> " " <> itemDesc
    cmpDesc = case ord of
      LT -> "less than"
      GT -> "greater than"
      EQ -> "equal to"
    f subj = case cmp subj of
      Nothing -> False
      Just ord' -> ord == ord'

greater
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> Predbox a
greater d a = compare d a GT

less
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> Predbox a
less d a = compare d a LT

equal
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> Predbox a
equal d a = compare d a EQ

greaterEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> Predbox a
greaterEq d a = greater d a ||| equal d a

lessEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> Predbox a
lessEq d a = less d a ||| equal d a

notEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> Predbox a
notEq d a = not $ equal d a

greaterBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Predbox a
greaterBy iD tD cmp = compareBy iD tD cmp GT

lessBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Predbox a
lessBy iD tD cmp = compareBy iD tD cmp LT

equalBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Predbox a
equalBy iD tD cmp = compareBy iD tD cmp EQ

greaterEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Predbox a
greaterEqBy iD tD cmp =
  greaterBy iD tD cmp ||| equalBy iD tD cmp

lessEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Predbox a
lessEqBy iD tD cmp =
  lessBy iD tD cmp ||| equalBy iD tD cmp

notEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> Predbox a
notEqBy iD tD cmp =
  not $ equalBy iD tD cmp

-- | Parses a string to find the correct comparer; returns the correct
-- function to build a Predbox.

parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed
  -> (Ordering -> Predbox a)
  -- ^ A function that, when given an ordering, returns a Predbox
  -> Maybe (Predbox a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- Predbox.
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

