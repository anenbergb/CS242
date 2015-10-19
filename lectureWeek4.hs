getLine :: IO [Char]
getLine = getChar >>= \c -> 
		case c of 
		'\n' -> return []
		_ -> getLine >>= \cs ->
			return (c:cs)


similar to 
f :: Char -> -> IO [Char]
f c = case c of
		'\n' -> return []
		_ -> getLine >>=
			(\cs -> return (c:cs))

--Also in Do notation
do{x<-m,s} == m >>=\x -> do {s}
do {e} == e

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

-- Another combinator
--forever
forever :: IO () -> IO ()
--forever m = m >> m >> m >> ...
forever m = m >> forever m
--  () is for the type unit. data Unit = Unit

repeatN :: Int -> IO () -> IO ()
repeatN 0 m = return ()
repeatN n m = m >> repeatN (n-1) m

--repeatN 10 (putStr "Hello")

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


forever (putStr "Hello")
--Pass putStr "Hello" as a recipe. but it isn't evaluated until later.

--for [1,2] (\i -> putStr "Hello")

for xs f = sequence (map f xs) >> return ()

--copy some more stuff from teh video.
sequence 




--MONADS
class Monad m where
	(>>=) :: m a -> (a -> m b ) -> m b
	return :: a -> m a

class HasMappable f where
	fmaps::(a->b) -> f a -> f b





