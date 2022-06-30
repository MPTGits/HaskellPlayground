{-Haskell playground-}

-- Test section for random stuff

myOp x y = 100

-- ----------------------------------------------------------------------------------
--Task from and for stateExam preperation

fibonaci:: Int -> Int
fibonaci 0 = 0
fibonaci 1 = 1
fibonaci x = fibonaci(x-1) + fibonaci(x-2)

lenght:: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1 + lenght(xs)

customMap:: [a] -> (a -> b) -> [b]
customMap [] f = []
customMap (x:xs) f = (f x) : customMap xs f


--myAny:: (a -> Bool) -> [a] -> Bool
--myAny f [] = False
--myAny f (x:xs) = (f x) || (myAny f xs)

myAny:: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs)
	|f x = True
	|otherwise = (myAny f xs)

myAll:: (a -> Bool) -> [a] -> Bool
myAll _ [] = False
myAll f [x] = (f x)
myAll f (x:xs) = (f x) && (myAll f xs)

myZip:: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y):(myZip xs ys)

--myAppend:: a -> [a] -> [a]
--myAppend x [] = [x]
--myAppend x xs = xs ++ [x]

myAppend:: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x: (myAppend xs ys)  


myFoldr:: (a->a->a) -> a -> [a] -> a
myFoldr f s [] = s
myFoldr f s (x:xs) = (myFoldr f (f x s) xs)

--State Exam 2014

--A)

totalMin:: [(Int->Int)] -> (Int->Int)
totalMin [] = (\x -> x)
totalMin l = foldr1 (\curr result -> if (curr 0) > (result 0) then result else curr) l

--B)

chainMinCompositions:: (Int->Int) -> [(Int->Int)]
chainMinCompositions f = helper f 0 (\x -> x) (\x -> f x)
	where
		helper:: (Int->Int) -> Int -> (Int->Int) -> (Int->Int) -> [(Int->Int)]
		helper f 0 f0 f1 = f0:(helper f 1 f0 f1)
		helper f 1 f0 f1 = f1:(helper f 2 f0 f1)
		helper f n f0 f1 = if (elem False [(f0 x) == (f1 x) | x <- [0..n]]) then (f0.f1):(helper f (n + 1) f1 (f0.f1))
			else (totalMin (take (n - 1) (helper f 0 (\x -> x) (\x -> f x)))):(helper f (n + 1) f1 (totalMin (take (n - 1) (helper f 0 (\x -> x) (\x -> f x)))))


--State Exam 2014.09.11



removeFirst:: (Eq a) => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) y = if x == y then xs else x:removeFirst xs y


perm :: Eq a => [a] -> [[a]]
perm [] = [[]]
perm xs = [y:tmp | y <- xs, tmp <- perm $ (removeFirst xs y)]


--State Exam 2015.09.10

merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = if x < y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

--State Exam 2015.07.14

-- (map (head [(\couple -> fst couple + snd couple)])  (foldr1 (++) [[(1,2)],[(3,4)]]))

--Answer: [3, 7]

-- [zip [x] [x] | x <- [1..5]]

--Answer: [[(1, 1)], [(2, 2)], [(3,3)], [(4,4)], [(5,5)]]

-- map (\(x:y:z)->x:z) [[1,2,3],[2,3,1],[3,1,2]]

--Answer:[[1, 3], [2, 1], [3, 2]]


--State Exam 2016.07.12

-- [ x : [x] | x <- [ [1,2], [3,4] ] ]

--Answer: [[[1, 2], [1 2]], [[3, 4], [3, 4]]]

-- [map (f 5) [1,2,3] | f <- [ (+), (-), (*) ] ]

--Answer: [[6 7 8], [4, 3 , 2], [5, 10, 15]]

-- "a" : [ ['b', 'c'], "d" ]

--Answer: ["a", "bc", "d"]

--State Exam 2016.09.09

filterByChar c ls = filter (\lst -> (elem c lst)) ls

-- let (x:y):z = ["Curry"] in (x, y, z)

--Answer: ('C',"urry",[])

--State Exam 13.07.2018

addDefault val [] = [val]
addDefault val l  = l

sumMinFix fl xl = foldr1 (+) (map (\f -> foldr1 min (addDefault 0 [x | x<- xl, (f x) == x])) fl)
