without:: (Eq a)=>[a] -> a -> [a]
without xs x = filter (/=x) xs

perm::(Eq a) => [a] -> [[a]]
perm [] = [[]]
perm xs =[x:rest| x <- xs,rest <- perm (xs `without` x) ]

--var xs k =[x:rest| x <- xs,rest <- var (xs `without` x) (k-1) ]
var::(Eq a)=>[a]->Int->[[a]]
var _ 0 = [[]]
var [] _ = [[]]
var xs k =[x:rest| x<-xs,rest <- var xs (k-1)]
--Main version of the variation 
{-
var xs k =concat $ map (\x -> map (\r -> x:r) rest x) prev
	where prev= var rest(k-1)
		  rest x= (xs `without` x)

var' xs k =[x:rest|x<-xs,rest <- var' (xs `without` x) (k-1) ]

-}

{-		
variation::(Eq a) => [a] -> a -> [[a]]
variation [] _ = [[]]
variation _ 0 = [[]]
variation xs n = [x:rest | x<-xs, rest <- perm ((take 2 xs) `without` x) ]
-}
--
--multiSet::(Ord a) => [a] -> [(a,Int)]
--multiSet xs = map (\l@(x:xs) -> (x,length l)) group.sort$l 

--Main solution 
multiSet::(Eq a) => [a] -> [(a,Int)]
multiSet [] = []
multiSet (x:xs) = (x , count x):multiSet (xs `without` x) 
	where count y = length $ filter (==y) (x:xs)
	
	
numOfRepResult::[(a,Int)] -> Int -> [a]
numOfRepResult [] _ = []
numOfRepResult _ 0 = [] 
numOfRepResult (x:xs) n = if snd x >= n then fst x:(numOfRepResult xs n) else (numOfRepResult xs n)  


--using the previous task make a function that joins the corresponding elements in a list
--moreLists [1, 2, 4, 2] [2, 3, 2, 7] -> [[1, 2], [2, 3], [4, 2], [2, 7]]
moreList::[a]->[a]->[[a]]
moreList [] _ = []
moreList _ [] = []
moreList (x:xs)(y:ys) = [x,y]:moreList xs ys 
	
	
myFoldl::(a -> b -> b) -> b -> [a] -> b
myFoldl _ y [] = y
myFoldl f y (x:xs) = myFoldl f (f x y) xs

takeWhile'::(a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs)  = if (f x) then x:takeWhile f xs else []

repeated :: (a -> a) -> Int -> (a -> a)
repeated f 1 = f
repeated f n = \x -> f (repeated f (n-1) x)

foo2 = map (+1) . map (^2) . map (+1) 

addNum x = \y -> x+y


fib::Int -> (Integer,Integer)
fib 0 = (0,1)
fib n = (snd (fib (n-1)),fst (fib (n-1)) + snd (fib (n-1)))

odds_fibs::Int->[Integer]
odds_fibs n = if ((fst (fib n) `mod` 2) == 0) then odds_fibs (n+1) else (fst (fib n)):odds_fibs (n+1)

--concat_odd_fib::Int->[Integer]->[Integer]
--concat_odd_fib n xs = (map (:) takeWhile (n<0) (:) xs):(concat_odd_fib (n+1) xs) 

--Zad 15
compose::(a->a) -> a -> Int -> [a]
compose _ _ 0 = []
compose f x n = x:compose f (f x) (n-1) 

--Pismen izpit 2015 
--zad 14
--expr::Float->[Float]
--expr n = [sqrt (x+ (expr (n-1)))|x <- [1,3..(2*n-1)]]
	--where tmp=2*n+1

--zad 13 
{-
isDessc::[Char] -> Bool
isDessc [x] = True
isDessc [] = True
isDessc (x:y:xs) = if x>=y then isDessc (y:xs) else False 
-}