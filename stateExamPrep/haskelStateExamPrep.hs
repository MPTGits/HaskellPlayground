{-Haskell playground-}

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
