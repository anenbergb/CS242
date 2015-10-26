module IOMonad where

import Prelude hiding (getLine, sequence)
import Data.IORef
import System.IO.Unsafe


get :: IO [Char]
getLine = getChar >>- \c -> 
          case of
            '\n' -> return []
            _ => getLine >>= \cs -> 
                return (c:cs)
-- Recursively gets the next character.

get :: IO [Char]
getLine = getChar >>- f

f :: Char -> IO [Char]
f c = case c of
      '\n' -> return []
      _ -> getLine >>= 
            (\cs -> return (c:cs))



getLine :: IO [Char]
getLine = do { c <- getChar;
               if c == '\n' then
                    return []
               else do { cs <- getLine;
                         return (c:cs) }
                         }



getLine :: IO [Char]
getLine = do
  {
  c <- getChar
  ; case c of 
    '\n' -> do return []
    _ -> do  {
      cs <- getLine
      return (c:cs);
  }
}



--forever m = m >> m >> m >> ...
--  () is for the type unit. data Unit = Unit

-- We can define our own "control structures"

forever :: IO () -> IO ()
forever a = a >> forever a

-- forever (getLine >>= \_ -> return ())

--For loop
for :: [a] -> (a -> IO b) -> IO ()
for [] f = return ()
for (x:xs) f = f x >> for xs f

-- Prelude > for [1,2,3] print
1
2
3
--Can bind an IO action to a variable.
let x = putStrLn "x"
let y = x >> x
Prelude> y
x
x

-- to evaluate an action does not cause it to become performed
-- action isn't performed until you execute it.




forever (putStr "Hello")
--Pass putStr "Hello" as a recipe. but it isn't evaluated until later.

--for [1,2] (\i -> putStr "Hello")

for xs f = sequence (map f xs) >> return ()


repeatN :: Int -> IO () -> IO ()
repeatN 0 a = return ()
repeatN n a = a >> repeatN (n-1) a


--repeatN 10 (putStr "Hello")
-- putStr doesn't return anything (it just prints to the console)


-- repeatN 10 (putchar 'x')

for :: [a] -> (a -> IO b) -> IO ()
for [] fa = return ()
for (x:xs) fa = fa x >> for xs fa

-- for [1..10] (\x -> putStr (show x))

sequence :: [IO a] -> IO [a]
sequence [] = return []
sequence (a:as) = do r <- a
                     rs <- sequence as
                     return (r:rs)

for' :: [a] -> (a -> IO b) -> IO ()
for' xs fa = sequence (map fa xs) >> return ()


-- FIRST CLASS ACTIONS let us write APPLICATION SPECIFIC CONTROL
-- STRUCTURES

-- Monads
--      type constructor M
--      bind :: M a -> (a -> M b) -> M b
--      unit :: a -> M a
-- plus some laws
--      unit x >>= f = f x
--      m >>= unit   = m
--      m1 >>= (\x -> m2 >>= \y -> m3) = (m1 >>= (\x -> m2)) >>= \y -> m3
--      e.g. (optional, third law in do notation)
--          do x <- do y <- m2
--                     m1
--             m3
--              ===
--          do y <- m2
--             x <- m1
--             m3
--
--
-- error handling monad / reader monad / state monad

-- Mutable references
--
-- :t newIORef
-- newIORef  :: a -> IO (IORef a)
readIORef x
0
-- :t readIORef
-- :t writeIORef
writeIORef :: IORef a -> a -> IO ()
writeIORef x 23
readIORef x
23



count :: Int -> IO Int
count n = do
    r <- newIORef 0
    addToN r 1
  where
    addToN :: IORef Int -> Int -> IO Int
    addToN r i | i > n = readIORef r
               | otherwise = do
                    v <- readIORef r
                    writeIORef r (v + i)
                    addToN r (i+1)

-- :t unsafePerformIO
r :: IORef a
r = unsafePerformIO (newIORef (error "urk"))

cast :: b -> c
cast x = unsafePerformIO (writeIORef r x >> readIORef r)

r is polymorphic, so thats why its unsafe.
 




--MONADS
class Monad m where
  (>>=) :: m a -> (a -> m b ) -> m b
  return :: a -> m a

class HasMappable f where
  fmaps::(a->b) -> f a -> f b



compose :: (a -> b) -> (b -> c) -> (a -> c)
compose = \x -> \y -> function body



