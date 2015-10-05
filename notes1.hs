
--Recursive thinking
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n<= 0		= []
	| otherwise	= x:replicate' (n-1) x

--Num isn't a subclass of Ord

--takes a certain number of elements from a list.
-- take 3 [5,4,3,2,1] returns [5,4,3]
--no otherwise means that the matching will fall throuhg
-- to the next pattern.
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0	= []
take' _ []		= []
take' n (x:xs) 	= x : take' (n-1) xs

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

--infinite list
repeat' :: a -> [a]  
repeat' x = x:repeat' x  


zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  


--takes an element and a list and sees if that element
-- is in the list.
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  


--Higher Order functions
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

--By calling functions with too few paramters we create
--new functions on the fly
multTwoWithNine = multThree 9
--this one only requires 2 arguments.


compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  


-- same as doing 200 / 10. as in doing (/10) 200
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) 


