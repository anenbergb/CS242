-- HOW to actually start using the Haskell interpretor.
-- ghci --package QuickCheck-2.6 Lec.hs



module Lec where
-- currying
foo :: Bool -> (Bool -> Bool)
foo x y = x || y

:t foo True
foo True :: Bool -> Bool
:t foo True False
foo True False :: Bool
(foo True) :: Bool
False

empty = []

-- Adds True to the beginning of list
True :[]
[True]
-- 'a' is a type of polymorphism
1:2:3:[]
[1,2,3]

1:2:(3:[])
[1,2,3]

--First function we want to write is len
-- the length of the list is 1 + the length
-- of the rest of the list following the first
-- element (xs)
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

--Map function
-- Takes a function (a->b) and a list of a's and
-- returns a list b done by applying the function
-- to all of them.
mp :: (a -> b) -> [a] -> [b]
mp f [] = []
mp f (x:xs) = f x : map f xs

--To use this function
mp (\x -> x + 2) [1,2,3]

--notation for functions of multiple arguments
app :: [a] -> [a] -> [a]
app [] ys = ys
app (x:xs) ys = x : app xs ys

--reverse
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

--accumulator. parameter you pass along to your function
-- which tracks what is going on.

rev [] rs = rs
rev (x:xs) rs = rev xs (x:rs)

-- rev [1,2,3,4] []
-- [4,3,2,1]

--can have a procedure
-- also can use a where clause
rev :: [a] -> [a]
rev xs = go xs []
	where
		go [] rs = rs
		go (x:xs) rs = go xs (x:rs)

--let binding method
rev :: [a] -> [a]
rev xs = 
	let go [] rs = rs
		go (x:xs) rs = go xs (x:rs)
	in go xs []

--list comprehension
let x = [1,2,3]
map (\x -> x * 2)

[x * 2| x <- xs]
--goes to [2,4,6]
let xs = [1,2,3,4]
[x * 2 | x <- xs, x 'mod' 4 == 0]
-- goes to [8]
[x * 2 | x <- xs, mod x 4 == 0]
-- goes to [8]

--strings are just lists
:t 'h'
'h'
['h','i']
"hi"

data Color = Red | Green | Blue
	deriving (Show)

-- pattern matching works
swizzle  :: Color -> Color
swizzle Red = Green
swizzle Green = Blue
swizzle Blue = Red

--want to pattern match inside the function definition
swizzle :: Color -> Color
swizzle c = 
	case c of
		Red -> Blue
		_ -> Green

--define a list of integers.
--sytax for constructors is that you have the constructor 'Cons'
-- and then a list of types that the constructor holds.
-- Cons holds 2 pieces of data, Int and IntList
data IntList = Nil | Cons Int IntList
	deriving (Show)

--Cons is a function. Int -> IntList -> IntLIst
-- You can pattern match on constructors.

data AlgDataType = Constr1 Int Bool
				| Constr2 Int
				| Constr3
-- can have many constructors that take different types

-- Tree can either be a leaf (integer) or a node (which Takes a tree and ouputs a tree)
data Tree = Leaf Int
		| Node Tree Tree
--or
data Tree = Leaf
		| Node Tree Int Tree
sumTree :: Tree -> Int
sumTree Leaf = 0
sumTree (Node tl x tr) = x + sumTree tl + sumTree tr

--Folds.
foldTree :: Tree -> Int -> (Int -> Int -> Int -> Int) -> Int
foldTree Leaf z f = z
foldTree (Node tl x tr) z f = 
	f (foldTree tl z f)
		x
		(foldTree tr z f)

--Lazy evaluation
-- If then else is usually built into the language.
ift :: Bool -> a -> a -> a
ift True t f = t
ift False t f = f

--Streams: repeat a.
-- streams are same thing as lists.
myrepeat :: a -> [a]
myrepeat x = x : myrepeat x

--We can define the Fibonacci Numbers in Haskell by using lazy streams.
f 0 = 0
f 1 = 1
f n = f (n-1) + f (n-2)

-- you would want to memoize the results of the computation.
-- we will use zipWith ::(a -> b -> c) -> [a] -> [b] -> [c]
-- takes a function, (a binary function a-> b-> c), a list of [a], and a list of [b], for the first element
-- of both of them, it runs f(a,b) to get a number c, which is the first element of the resulting list

--tail: gives you the list without its first element
--take :: Int -> [a] -> [a]
-- take 2 [3,4,5] gives you [3,4]
fibs = 0:1:zipWidth (+) fibs (tail fibs)

-- the fibonacci sequence is like if you have the fibonacci sequence and then you add it with the 
-- fibonacci sequence shifted one index forwards.




--How to use Maybe. The type Maybe a means you either have Just an a, or you have Nothing.
-- Here is some example code demonstrating:
 f :: Int -> Maybe Int
 f 0 = Nothing
 f x = Just x
 
 g :: Int -> Maybe Int
 g 100 = Nothing
 g x = Just x
 
 h :: Int -> Maybe Int
 h x = case f x of
         Just n -> g n
         Nothing -> Nothing
--Pattern matches can be NESTED. For example, a pattern match on a list for match for multiple elements at once:
tell :: [Int] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

--Immutability in Haskell. In many of the list manipulation functions I mentioned in lecture,
-- I glossed over what happened to the original list. In fact, in Haskell, whenever I do an operation,
-- a NEW list is created (the old one stays unchanged). This is, of course, because Haskell is pure,
-- and we do not have any mutation by default.