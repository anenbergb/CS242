{-# LANGUAGE OverloadedStrings, OverlappingInstances, FlexibleInstances, FlexibleContexts #-}
module Lambda where

import Test.QuickCheck
import Test.QuickCheck.Gen

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char
import Data.String

import Data.List

-- In this lab, we will implement a small-step evaluator for the lambda
-- calculus using substitution.  While the "stated" goal of this lab is
-- to give you some experience implementing substitution (an important
-- ingredient for any real world compiler of a functional programming
-- language), there is also a hidden goal giving you an example of
-- "threading state" which will be helpful motivation for monads,
-- which we will look at later in the course.
--
-- You may find it convenient to call GHCi with the following flag:
--
--      ghci -XOverloadedStrings Lambda.hs
--
-- This enables a syntax-extension, overloaded strings, which will
-- allow you to type Haskell-style "\\x -> x" for a lambda expression,
-- instead of having to type out Lambda "x" (Var "x").  (Note
-- that if you are typing a String literal, backslashes must be
-- escaped.)
--
-- We must first start by defining a data type for our lambda calculus.
-- It is a very simple data type which directly reflects the grammar
-- for the lambda calculus.  We represent names as simple Haskell
-- strings.

data Expr = Var Name
          | Lambda Name Expr
          | App Expr Expr
    deriving (Eq)

type Name = String

-- Write a function "free variables" which computes a set of
-- the free variables of a lambda term.  Sets can be manipulated
-- using the following functions, which "do the obvious thing".
--
--      Set.empty     :: Set a
--      Set.singleton :: a -> Set a
--      Set.delete    :: a -> Set a -> Set a
--      Set.union     :: Set a -> Set a -> Set a
--      Set.unions    :: [Set a] -> Set a
--      Set.member    :: a -> Set a -> Bool
--
-- The full documentation for Set functions can be found here:
-- http://hackage.haskell.org/package/containers/docs/Data-Set.html
-- You may also find the Hoogle tool useful:
-- https://www.haskell.org/hoogle/
--
-- (Haskell tip: the period in these names are a qualifier, which help
-- us disambiguate functions which operate on Sets from functions which
-- operate on Maps, which we will use later.  Just pretend Set.delete is
-- a function like setDelete would be.)
--
-- (Haskell tip: Actually, these functions have slightly more
-- complicated type signatures, which you can check in GHCi.  The "Ord a"
-- is a constraint which says that "a" must be a *comparable* type.
-- We will talk more about this in the type classes lecture.)

fv :: Expr -> Set Name
-- BEGIN fv (DO NOT DELETE THIS LINE)
--error: equations for fv have different number of arguments.
fv (Var x:xs)
    | x == " " = fv xs
    | otherwise = Set.union (Set.singleton x) (fv xs)
--assume name is just a single variable, like "x"
fv (Lambda n e)
    | Set.member n r  = Set.delete n r
    | otherwise = r
    where r = fv e
fv (App e1 e2) = Set.union (fv e1) (fv e2)
-- END fv (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Substitution

-------------------------------------------------------------------

-- Let us define a substitution to be a mapping of variables to
-- expressions.  A 'Map k a' is a map from names to expression.  As with
-- sets, there are a few useful functions for manipulating these:
--
--      Map.lookup :: k -> Map k a -> Maybe a
--      Map.insert :: k -> a -> Map k a -> Map k a
--      Map.delete :: k -> Map k a -> Map k a
--      Map.elems  :: Map k a -> [a]
--      Map.empty  :: Map k a
--
-- (These types have also been similarly simplified.)

type Subst = Map Name Expr

emptySubst :: Subst
emptySubst = Map.empty

mkSubst :: Name -> Expr -> Subst
mkSubst = Map.singleton

-- The first substitution function we will define is the dumbest
-- implementation of substitution, which ALWAYS changes a binder to
-- something fresh when substituting.
--
-- How do we know what a fresh variable is?  In an impure language,
-- we might maintain a global counter i, such that x_i, x_i+1, ...
-- are guaranteed to be fresh: every time a name is allocated,
-- this counter is bumped.  However, Haskell does not allow this
-- sort of mutation, so instead we must manually thread this global
-- counter through our code.  To avoid confusion, let's define a
-- type synonym for our counter, 'FreshSupply'.

type FreshSupply = Int

-- Every function that may need fresh names must accept a 'FreshSupply'
-- as its argument (so that it can tell what names are free), and return
-- a new 'FreshSupply' which is updated to exclude and names which were
-- used.  So for example, the function 'freshDumb' which allocates a
-- new fresh name takes a 'FreshSupply', and returns both a fresh
-- 'Name' (from 'FreshSupply'), as well as 'FreshSupply' + 1 (indicating
-- what the next new fresh name is.)  This function relies on an
-- invariant that no user-input variables start with a dollar sign
-- (unfortunately, this also means if you pretty-print these variables
-- they can't be parsed).

freshDumb :: FreshSupply -> (Name, FreshSupply)
freshDumb u = ("$u" ++ show u, u+1)

-- Implement 'substDumb', which takes a 'FreshSupply', expression and
-- a substitution, and returns the substituted expression along with
-- a new 'FreshSupply'.

-- BEGIN substDumb (DO NOT DELETE THIS LINE)
substDumb = undefined
-- END substDumb (DO NOT DELETE THIS LINE)

-- This strategy is pretty annoying:
--
--      1. We doing loads and loads of allocation for all of these
--      fresh variables, and they might not even be needed!
--
--      2. We have to manually thread through the 'FreshSupply' through
--      all of our calls, which is a pain.
--
-- Here is an alternative: instead of threading the 'FreshSupply' state
-- through all of our functions, we should instead just determine a
-- complete set of names which are not fresh, and then just pick a name
-- which is not in this set.  For example, in the underlined
-- subexpression:
--
--      \x -> (\y -> x) z
--            ---------
--
-- If we are renaming y, we only have to avoid picking 'x', which is
-- in the free variables of the body of the lambda expression; any
-- other choice (including z!) is acceptable.
--
-- In lecture, we gave a precise condition pair of conditions:
--
--  1. It is not necessary to rename if the binder is not in the
--     free variables of the substitution.
--
--  2. If you need to rename, the newly chosen binder must not be in the
--     free variables of the inner lambda expression or the
--     substitution.
--
-- If you like, try modifying substDumb so that it attempts to rename
-- less (but don't change the signature of the function).  If you did
-- so, you will notice is that you had to call 'fv' repeatedly in order
-- to compute the set of "not fresh" variables.  This is bad for
-- performance!
--
-- A better strategy is to APPROXIMATE the set of unfresh variables (so
-- that it never omits an unfresh variable, but may contain actually
-- fresh variables.)  A convenient approximation is to have this set be
-- the set of variables which are "in scope".

type InScope = Set Name

-- In the previous example:
--
--      \x -> (\y -> x) z
--            ---------
--
-- In the underlined lambda expression, the variables x and z are in
-- scope (x because it was bound by a lambda, z because it's free in the
-- program), so they are not permissible choices.  So all we have to do
-- is pick a variable which is not in the in-scope set.  Here's
-- a function that does that:

fresh :: InScope
      -> Name -- The previous name
      -> Name -- A new name not in the InScope set.
fresh in_scope x = head (filter (`Set.notMember` in_scope) cands)
    where
        -- There are better strategies for generating a list of
        -- candidate variables (this strategy could end up forcing
        -- us to try a lot of variables before we find a fresh one),
        -- but this will be adequate for this lab.
        cands = map ((takeWhile isAlpha x ++) . show) [1..]

-- Implement substitution using an "in-scope" set.  (Hint: a full
-- description of how to do it is "Secrets of the Glasgow Haskell
-- Compiler inliner.")

substScope :: Expr -> Subst -> InScope -> Expr
-- BEGIN substScope (DO NOT DELETE THIS LINE)
substScope = undefined
-- END substScope (DO NOT DELETE THIS LINE)

-- An easy way to get the initial in-scope set given just a
-- substitution and an expression is to just take the free
-- variables of the substitution and the expression.

subst :: Expr -> Subst -> Expr
subst e s = substScope e s (Set.union (fv e) (Set.unions (map fv (Map.elems s))))

-------------------------------------------------------------------

--                  Alpha equivalence

-------------------------------------------------------------------

-- We should write some tests for our substitution functions.  However,
-- most properties we'd like to test require alpha-equivalence.
-- Haskell's default equality will not work: it will claim that "\\x ->
-- x" is not equal to "\\y -> y".  Please implement an alpha-equivalence
-- test.
--
-- (Hint: You CAN do this by direct recursion on e1 and e2, but it's
-- easy to accidentally end up with quadratic runtime.  A better idea is
-- to write a helper function that also takes as an argument a pair of
-- Maps tracking the binders you've seen so far for each expression.
-- Your helper function might have a type like:
-- 'Int -> Expr -> Map Name Int -> Expr -> Map Name Int -> Bool')

alphaEq :: Expr -> Expr -> Bool
-- BEGIN alphaEq (DO NOT DELETE THIS LINE)
alphaEq = undefined
-- END alphaEq (DO NOT DELETE THIS LINE)

-- Here are some examples to test on. (Question: which of these
-- should alphaEq return True for?)

example_alphaEq1 = alphaEq "\\x y -> x" "\\y x -> y"
example_alphaEq2 = alphaEq "(\\x -> x) z" "(\\z -> z) x"

-- For the next part, please write down three (or more) QuickCheck
-- properties testing 'alphaEq', 'substDumb' and/or 'subst'.  Your
-- properties should be only 1-2 lines long (not including type
-- signatures), or longer if you define a WELL-SPECIFIED helper
-- function (the helper function does something well defined and
-- easy to understand).  Your properties can take any number of 'Expr's and
-- 'Subst's as arguments, and QuickCheck will generate random inputs
-- for all of them.  For example, you could write a property with
-- the type 'Expr -> Subst -> Bool'.

-- Here is an example property to give you some idea of what kind of
-- properties we're looking for.  They don't have to be complicated!
prop_alphaEq_refl :: Expr -> Bool -- Alpha equivalence is symmetric
prop_alphaEq_refl e = alphaEq e e

-- A good QuickCheck property is one that can uncover bugs in your
-- implementation.  You might find this website useful if you want
-- to use some more features of QuickCheck:
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
-- (although there are plenty of properties which can be done in
-- "plain old style.")

-- BEGIN QuickCheck (DO NOT DELETE THIS LINE)
-- Your properties here
-- END QuickCheck (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Reduction

-------------------------------------------------------------------

-- Enough work, time to play. Implement a function which takes
-- a CBN lambda term, and evaluates a single step.  As a reminder,
-- here are the small-step operational semantics for CBN:
--
--      (\x -> e1) e2  ---->  e1[x -> e2]
--
--                 e1  ---->  e1'
--              --------------------
--              e1 e2  ---->  e1' e2

step_cbn :: Expr -> Maybe Expr
-- BEGIN step_cbn (DO NOT DELETE THIS LINE)
step_cbn = undefined
-- END step_cbn (DO NOT DELETE THIS LINE)

-- With a step function, we can write a function which takes an
-- expression, and returns the (possibly infinite) sequence of
-- reductions it takes until no more steps are possible.  For example:
--
--      reductions step_cbn "(\\x -> (\\z -> z) x) y"
--          == [ "(\\x -> (\\z -> z) x) y"
--             , "(\\z -> z) y"
--             , "y"
--             ] -- up to alpha equivalence
--
-- Implement this function.

reductions :: (Expr -> Maybe Expr) -> Expr -> [Expr]
-- BEGIN reductions (DO NOT DELETE THIS LINE)
reductions = undefined
-- END reductions (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Other stuff

-------------------------------------------------------------------

instance Show Expr where
    show e = show (pprExpr e)

instance IsString Expr where
    fromString = parseExpr

prop_parsePpr :: Expr -> Bool
prop_parsePpr e = parseExpr (pprExpr e) == e

-- A pretty-printer.  The integer parameter to 'go' is the "precedence
-- level", we use it to control parenthesization.
pprExpr :: Expr -> String
pprExpr = go 0
    where go n e = paren n (go' e)
          go' (Var x)      = (9, x)
          go' (App e1 e2)  = (6, go 5 e1 ++ " " ++ go 6 e2)
          go' (Lambda x e) = (3, "\\" ++ x ++ " -> " ++ go 2 e)
          paren n (m, s) | m > n     = s
                         | otherwise = "(" ++ s ++ ")"

parseExpr :: String -> Expr
parseExpr s =
  case readP_to_S exprParser s of
    [(a, "")] -> a
    _ -> error "parseExpr: failed"

exprParser :: ReadP Expr
exprParser = do e <- expr
                eof
                return e
    where expr = fun <++ app <++ nonApp
          fun =
            do string "\\"
               xs <- many1 (skipSpaces >> var)
               skipSpaces
               string "->"
               skipSpaces
               e <- expr
               return (foldr Lambda e xs)
          nonApp =
            between (char '(') (char ')') expr <++
            do x <- var
               return (Var x)
          app =
            do e  <- nonApp
               es <- many1 (skipSpaces >> nonApp)
               return (foldl App e es)
          var =
             do c <- satisfy (\c -> isLower c || c `elem` ("_" :: String))
                cs <- munch (\c -> isAlphaNum c || c `elem` ("\'_" :: String))
                return (c:cs)

instance Arbitrary (Map Name Expr) where
    arbitrary = (Map.fromList . zip candidateNames) `fmap` listOf arbitrary

-- Thanks ehird! http://stackoverflow.com/q/9542313/23845
candidateNames = [1..] >>= (`replicateM` ['a'..'z'])

instance Arbitrary Expr where
    arbitrary = go Set.empty
        where go :: InScope -> Gen Expr
              go in_scope =
                sized $ \n ->
                  if n == 0
                    then frequency [(1, fmap Var free_var),
                                    (4, fmap Var bound_var)]
                    else resize (n `div` 2) $
                         frequency [(6, redex),
                                    (1, lambda free_var),
                                    (1, lambda bound_var),
                                    (1, app),
                                    (1, fmap Var free_var),
                                    (1, fmap Var bound_var)]
                where redex = do x <- free_var
                                 e1 <- go (Set.insert x in_scope)
                                 e2 <- go in_scope
                                 return (App (Lambda x e1) e2)
                      lambda gen_var = do x <- gen_var
                                          e <- go in_scope
                                          return (Lambda x e)
                      app = do e1 <- go in_scope
                               e2 <- go in_scope
                               return (App e1 e2)
                      free_var  = return (head (filter (`Set.notMember` in_scope) candidateNames))
                      bound_var
                        | Set.null in_scope = free_var
                        | otherwise = oneof (map return (Set.toList in_scope))
    shrink (Var _) = []
    shrink (Lambda x e) = [e] ++ [Lambda x e' | e' <- shrink e]
    shrink (App e1 e2) = [e1, e2] ++ [App e1' e2 | e1' <- shrink e1]
                                  ++ [App e1 e2' | e2' <- shrink e2]

check :: Testable prop => prop -> IO ()
check = quickCheckWith stdArgs {maxSuccess = 1000}
