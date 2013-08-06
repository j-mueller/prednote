{-# LANGUAGE OverloadedStrings #-}

-- | Trees of predicates.
--
-- Exports names which conflict with Prelude names, so you probably
-- want to import this module qualified.

module Data.Prednote.Pdct

  ( -- * The Pdct tree
    Label
  , Hide
  , Pdct(..)
  , Node(..)
  , rename

  -- * Creating operands
  , operand

  -- * Creating Pdct.
  -- | All functions create Pdct that are shown by default.
  , and
  , or
  , not
  , (&&&)
  , (|||)
  , always
  , never
  , boxPdct
  , boxNode

  -- * Controlling whether Pdct are shown in the results
  , hide
  , show
  , hideTrue
  , hideFalse

  -- * Result
  , Result(..)
  , RNode(..)
  , evaluate
  , evaluateNode
  , IndentAmt
  , Level
  , showResult
  , showTopResult

  -- * Showing and evaluating Pdct
  , showPdct

  -- * Helpers for building common Pdct
  -- ** Non-overloaded
  , compareBy
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

import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat, mempty)
import Data.String (fromString)
import qualified System.Console.Rainbow as R
import Prelude hiding (not, and, or, compare, filter)
import qualified Prelude

-- # Pdct type

type Label = Text

-- | Determines whether a result is shown by default.
type Hide = Bool

-- | A predicate. Each Pdct contains a tree of Node.
data Pdct a = Pdct
  { pLabel :: Label
  -- ^ Label used when showing the results

  , pHide :: (Bool -> Hide)
  -- ^ As results are computed, this function is applied to the
  -- result. If this function returns False, then this Pdct will not
  -- be shown by default in the results.

  , pNode :: Node a

  }

data Node a
  = And [Pdct a]
  -- ^ Conjunction. If any Pdct in the list is False, the result is
  -- False. If the list is empty, the result is True.

  | Or [Pdct a]
  -- ^ Disjunction. If at least one Pdct in the list is True, the
  -- result it True. If the list is empty, the result is False.

  | Not (Pdct a)
  -- ^ Negation

  | Operand (a -> Bool)
  -- ^ Most basic building block.

-- | Renames the top level of the Pdct. The function you pass will be
-- applied to the old name.
rename :: (Text -> Text) -> Pdct a -> Pdct a
rename f p = p { pLabel = f (pLabel p) }

-- | Always True
always :: Pdct a
always = Pdct "always True" (const False) (Operand (const True))

-- | Always False
never :: Pdct a
never = Pdct "always False" (const False) (Operand (const False))

-- | Creates and labels operands.
operand :: Label -> (a -> Bool) -> Pdct a
operand l = Pdct l (const False) . Operand

-- | Creates And Pdct using a generic name
and :: [Pdct a] -> Pdct a
and = Pdct "and" (const False) . And

-- | Creates Or Pdct using a generic name
or :: [Pdct a] -> Pdct a
or = Pdct "or" (const False) . Or

-- | Creates Not Pdct using a generic name
not :: Pdct a -> Pdct a
not = Pdct "not" (const False) . Not

-- | Changes a Pdct so it is always hidden by default.
hide :: Pdct a -> Pdct a
hide p = p { pHide = const True }

showAll :: Pdct a -> Pdct a
showAll p = p { pHide = const False }

hideTrue :: Pdct a -> Pdct a
hideTrue p = p { pHide = id }

hideFalse :: Pdct a -> Pdct a
hideFalse p = p { pHide = Prelude.not }

-- | Forms a Pdct using 'and'.
(&&&) :: Pdct a -> Pdct a -> Pdct a
(&&&) x y = Pdct "and" (const False) (And [x, y])
infixr 3 &&&

-- | Forms a Pdct using 'or'.
(|||) :: Pdct a -> Pdct a -> Pdct a
(|||) x y = Pdct "or" (const False) (Or [x, y])
infixr 2 |||

-- | Given a function that un-boxes values of type b, changes a Pdct
-- from type a to type b.
boxPdct
  :: (b -> a)
  -> Pdct a
  -> Pdct b
boxPdct f (Pdct l d n) = Pdct l d $ boxNode f n

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
  Operand g -> Operand $ \b -> g (f b)

-- # Result

data Result = Result
  { rLabel :: Label
  , rBool :: Bool
  , rHide :: Hide
  , rNode :: RNode
  } deriving (Eq, Show)

data RNode
  = RAnd [Result]
  | ROr [Result]
  | RNot Result
  | ROperand Bool
  deriving (Eq, Show)

evaluate :: a -> Pdct a -> Result
evaluate a (Pdct l d n) = Result l r d' rn
  where
    rn = evaluateNode a n
    r = case rn of
      RAnd ls -> all rBool ls
      ROr ls -> any rBool ls
      RNot x -> Prelude.not . rBool $ x
      ROperand b -> b
    d' = d r

evaluateNode :: a -> Node a -> RNode
evaluateNode a n = case n of
  And ls -> RAnd (map (evaluate a) ls)
  Or ls -> ROr (map (evaluate a) ls)
  Not l -> RNot (evaluate a l)
  Operand f -> ROperand (f a)

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

-- # Showing Pdct

-- | Creates a plain Chunk from a Text.
plain :: Text -> R.Chunk
plain = R.Chunk mempty

-- | Shows a Pdct tree without evaluating it.
showPdct :: IndentAmt -> Level -> Pdct a -> [R.Chunk]
showPdct amt lvl (Pdct l _ pd) = case pd of
  And ls -> indent amt lvl [plain ("and - " <> l)]
            <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Or ls -> indent amt lvl [plain ("or - " <> l)]
           <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Not t -> indent amt lvl [plain ("not - " <> l)]
           <> showPdct amt (lvl + 1) t
  Operand _ -> indent amt lvl [plain ("operand - " <> l)]

instance Show (Pdct a) where
  show = X.unpack
       . X.concat
       . map R._text
       . showPdct 2 0

-- # Showing Result

labelBool :: Text -> Bool -> [R.Chunk]
labelBool t b = [open, trueFalse, close, blank, txt]
  where
    trueFalse = 
      if b then "TRUE" <> R.f_green else "FALSE" <> R.f_red
    open = "["
    close = "]"
    blank = plain (X.replicate blankLen " ")
    blankLen = X.length "discard"
               - X.length (R._text trueFalse) + 1
    txt = plain t

type ShowAll = Bool

showResult
  :: IndentAmt
  -> ShowAll
  -> Level
  -> Result
  -> [R.Chunk]
showResult amt sa lvl (Result lbl rslt hide nd)
  | hide && Prelude.not sa = []
  | otherwise = firstLine ++ restLines
  where
    firstLine = indent amt lvl $ labelBool lbl rslt
    restLines = case nd of
      RAnd ls -> f False ls
      ROr ls -> f True ls
      RNot r -> showResult amt sa (lvl + 1) r
      ROperand _ -> []
    f stopOn ls = concatMap sr ls' ++ end
      where
        ls' = takeThrough ((== stopOn) . rBool) ls
        sr = showResult amt sa (lvl + 1)
        end = if ls' `shorter` ls
              then indent amt (lvl + 1) ["(short circuit)"]
              else []

shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough f (x:xs) = x : if f x then [] else takeThrough f xs

showTopResult
  :: X.Text
  -> IndentAmt
  -> ShowAll
  -> Result
  -> [R.Chunk]
showTopResult txt i sd r = showResult i sd 0 r'
  where
    r' = r { rLabel = rLabel r <> " - " <> txt }


-- # Comparisons

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

compareBy itemDesc typeDesc cmp ord = Pdct l (const False) (Operand f)
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

