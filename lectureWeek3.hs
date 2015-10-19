{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}
module TypeClass where

import Prelude hiding (map)

-- Syntax reminder for infix
-- + versus (+); how to define this way

x ==> y = if not x then True else y

-- Syntax: records

data Person = Person { name :: String, age :: Int }
exPerson = Person "Bob" 27
-- :t name

data Annot i a = Annot { annotation :: i, value :: a }
exAnnot = Annot { annotation = "boom", value = 2 }
-- :t value

-- Basic type classes: Show, Read, Eq, Ord, Num

-- :t show
-- (\x -> x) -- error message

-- The Num example
{-
:info Num
class Num a where
    (+) :: a -> a -> a

Any type a which has a Num instance

--Dictionary
This is a record
data Num a = MkNum {
                plus :: a -> a -> a,
                mul :: a -> a -> a,
                negate :: a -> a
                }
data Num a = MkNum {
                (+) :: a -> a -> a,
                (*) :: a -> a -> a,
                negate :: a -> a
                }Plus is something that takes a num dictionary, an a, a, and outputs an a.

Name of the record is plus,  the type it is supposed to have
is a -> a -> a

Originally
:t (+)
(+) :: Num a => a -> a -> a



In the dictionary world
:t (+)
(+) :: Num a -> a -> a -> a

Now we have to pass in a dictionary.
square :: Num a -> a -> a
square d x = (*) d x x

d is an extra argument of data type Num n. A value of type num T is a vector
of Num operations for T

We get the multiply operation from the dictionary
that we passed into the function.

dNumInt :: Num Int
dNumInt = MkNum plusInt mulInt neg Int

Need two numeric dictionaries
-- squares :: (Num a, Num b) => a -> b -> (a,b)
squares :: Num a -> Num b -> a -> b -> (a,b)
squares dx dy x y = (square dx, square dy y)

Didn't have to speficy the type of each square.
The dictionary indirects this away.


can test for equality of lists as long 
it is possible to test for equality on
the elements inside.

Can also do this for pairs.
instance (Eq a, Eq b) => Eq (a, b) where (x1,x2)==(x2,y2)  = x1==x2 & y1==y2

You can provide a default implementation for the method,



When you write,
-- instance Eq Bool where...
its same as
data Eq a = MkEq { (==} :: a -> a -> Bool }

dEqBool :: Eq Bool
dEqBool = MkEq { (==) = eq }
    where eq True True = True
          eq False False = True
          eq _ _ = False

What does this look like
--instance Eq a => Eq [a] where ...

Takes a dictionary on items and then transforms it into a dictionary for equality
on lists.

dEqList :: Eq -> Eq [a]
a

When I have an instance declaration taht has a constarint on some types, those types
just get interpreted as dictionaries taht get passed in which are used to build
the types for the larger class.


-- instance (Eq a, Eq b) => Eq (a, b) where ...

dEqPair :: Eq a -> Eq b -> Eq (a,b)
dEqPair da db = MkEq eq
    where eq (x1,y1) (x2,y2) = (==) da x1 x2 && (==) db y1 y2

Ord is the typeclass to compare to elements to see if one is graeter than the other.
    An ord instance automatically requires an Equal instance.

data Ord a = MkOrd { (<=) :: a -> a -> Bool,
                     E}

-}

square :: Num a => a -> a
square x = x * x
-- :t sort
-- :t serialize
-- :t member

-- :info Num

plusInt, mulInt :: Int -> Int -> Int
plusInt = (Prelude.+)
mulInt  = (Prelude.*)
negInt :: Int -> Int
negInt = (Prelude.negate)

{-
instance Num Int where
    x + y = plusInt x y -- plusInt some built-in
    x * y = mulInt x y
    negate x = negInt x
-}

squares :: (Num a, Num b) => a -> b -> (a, b)
squares x y = (square x, square y)

sumSq :: Num n => n -> n -> n
sumSq x y = square x + square y

-- Eq example

-- :info Eq

{-
instance Eq a => Eq [a] where
    (==) []     []     = True
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _      _      = False

instance (Eq a, Eq b) => Eq (a, b) where
    (a1, b1) == (a2, b2) = a1 == a2 && b1 == b2
-}

-- Overloaded constants example

inc :: Num a => a -> a
inc x = x + 1

-- Other remarks:
--  3 + "2"
--  :t 2

-- Ord example (for superclasses)

-- :info Ord

mycompare :: Ord a => a -> a -> Ordering
mycompare x y = if x == y then EQ
                    else if x <= y
                            then LT
                            else GT


-- Constructor classes
-- :t map
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

treeEx = Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)
treeExMap = mapTree (\x -> x + 1) treeEx

-- :info Maybe

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- All could be written as map :: (a -> b) -> g a -> g b

class HasMap g where
    map :: (a -> b) -> g a -> g b

instance HasMap [] where
    map f [] = []
    map f (x:xs) = f x : map f xs

instance HasMap Tree where
    map = mapTree

instance HasMap Maybe where
    map = mapMaybe

{-
:kind Int
Int :: *

:kind Maybe
MAybe :: * -> *

--type constructors. take a type, and return a type.


:kind Maybe Int
Maybe Int :: *


The function is insensitive to the types stored in it
data HasMap g = MkHasMap {
    map :: forall a b. (a -> b) -> g a -> g b
    }
`


-}



