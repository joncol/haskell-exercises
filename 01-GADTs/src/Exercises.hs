{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Exercises where
import Control.Arrow ((&&&))
import Data.List.NonEmpty (groupAllWith1)





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountableNil :: CountableList
  CountableCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountableNil = 0
countList (CountableCons x xs) = count x + countList xs


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountableNil = CountableNil
dropZero (CountableCons x xs)
  | count x == 0 = dropZero xs
  | otherwise = CountableCons x $ dropZero xs


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts =
  error "Impossible, since we know nothing about the type inside the \
        \'CountableList'"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList xs = go AnyNil xs
  where
    go :: AnyList -> AnyList -> AnyList
    go acc AnyNil = acc
    go acc (AnyCons x xs) = go (AnyCons x acc) xs

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = error "Impossible to implement, since we cannot access the \
                      \type of elements of the AnyList, so we cannot call the \
                      \predicate for elements."

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil = 0
lengthAnyList (AnyCons _ xs) = 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
foldAnyList =
  error "Impossible, we cannot assume that the elements are Monoids."

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList (AnyCons _ _) = False

instance Show AnyList where
  show = error "Impossible to implement, since we cannot assume that the \
               \elements of the AnyList have Show instances"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- `input` is an existential variable. We can only transform it to our output
-- type.

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq a => Eq (TransformableTo a) where
  TransformWith f x == TransformWith g y = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
instance Functor TransformableTo where
  fmap f (TransformWith g x) = TransformWith (f . g) x





{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

eqPairEq :: EqPair -> Bool
eqPairEq (EqPair x y) = x == y

eqPairNotEq :: EqPair -> Bool
eqPairNotEq (EqPair x y) = x /= y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

data EqPair'' a = Eq a => EqPair'' a a

eqPairEq'' :: EqPair'' a -> Bool
eqPairEq'' (EqPair'' x y) = x == y



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox n _)) = n

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ _) = 1
countLayers (StringBox _ _) = 2
countLayers (BoolBox _ _) = 3

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- Impossible to implement, since we cannot know the return type.
-- removeLayer :: MysteryBox a -> MysteryBox b
-- removeLayer EmptyBox = ()
-- removeLayer (IntBox n eb) = eb
-- removeLayer (StringBox s ib) = ib
-- removeLayer (BoolBox b sb) = sb

-- Clever way copied from the answers.
data Layer a b where -- We can use a GADT to encode the layers...
  Int'    :: Layer Int ()
  String' :: Layer String Int
  Bool'   :: Layer Bool String

-- And now we can write this function:
unpeel :: Layer a b -> MysteryBox a -> MysteryBox b
unpeel Int'    (IntBox    _ xs) = xs
unpeel String' (StringBox _ xs) = xs
unpeel Bool'   (BoolBox   _ xs) = xs

{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hHead :: HList (h, t) -> h
hHead (HCons h _) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = error "not possible to pattern match this"

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

append :: HList a -> HList b -> HList c
append  =
  error "cannot implement this because we cannot figure out the return type"


{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: HTree l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

delLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
delLeft (HBranch _ c r) = HBranch HEmpty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  HBranch l c r == HBranch l' c' r' = l == l' && c == c' && r == r'



{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil = []
-- getFirsts (ACons x ANil) = [x]
-- getFirsts (ACons x (ACons _ xs)) = x : getFirsts xs

-- Nice mutual recursion stolen from answer:
getFirsts (ACons x xs) = x : getSeconds xs

getSeconds :: AlternatingList a b -> [b]
getSeconds ANil = []
getSeconds (ACons _ xs) = getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues = (mconcat . getFirsts) &&& (mconcat . getSeconds)

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' ANil = (mempty, mempty)
-- foldValues' (ACons x ANil) = (x, mempty)
-- foldValues' (ACons x (ACons y ys)) = (x, y) <> foldValues ys

-- Alternative solution stolen from answer:
foldValues' (ACons x xs) =
  let (b, a) = foldValues' xs
  in (x <> a, b)





{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals e e') = eval e == eval e'
eval (Add e e') = eval e + eval e'
eval (If cond e e') = if eval cond then eval e else eval e'
eval (IntValue n) = n
eval (BoolValue b) = b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

data Typed where
  IntType :: Expr Int -> Typed
  BoolType :: Expr Bool -> Typed

clean :: DirtyExpr -> Maybe Typed
clean (DirtyEquals de de') = case (clean de, clean de') of
  (Just (IntType e), Just (IntType e')) -> Just . BoolType $ Equals e e'
  _ -> Nothing
clean (DirtyAdd de de') = case (clean de, clean de') of
  (Just (IntType e), Just (IntType e')) -> Just . IntType $ Add e e'
  _ -> Nothing
clean (DirtyIf dcond de de') = case (clean dcond, clean de, clean de') of
  (Just (BoolType cond), Just (IntType e), Just (IntType e'))
    -> Just . IntType $ If cond e e'
  (Just (BoolType cond), Just (BoolType e), Just (BoolType e'))
    -> Just . BoolType $ If cond e e'
  _ -> Nothing
clean (DirtyIntValue n) = Just . IntType $ IntValue n
clean (DirtyBoolValue b) =  Just . BoolType $ BoolValue b

parse :: DirtyExpr -> Maybe (Expr Int)
parse de = case clean de of
  Just (IntType e) -> Just e
  _ -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?



{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TANil :: TypeAlignedList a a
  TACons :: (a -> c) -> TypeAlignedList c b -> TypeAlignedList a b

-- | b. Which types are existential?
-- The types `(a -> c)` and `TypeAlignedList c b` are existential, since they
-- only occur on the left side in the constructor.

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs ys TANil = ys
composeTALs ys (TACons x xs) = TACons x $ composeTALs ys xs
