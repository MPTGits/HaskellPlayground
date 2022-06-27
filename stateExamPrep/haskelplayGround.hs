-- mySum :: Int -> Int -> Int
-- mySum x y = x + y


-- test :: Int -> Int
-- test x = x^2

-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib(n-1) + fib(n-2)

-- len :: [a] -> Int
-- len [] = 0
-- len (x:xs) = 1 + len xs 

-- test2 :: Int -> Int
-- test2 x = x*2

-- myMap :: (a -> b) -> [a] -> [b]
-- myMap f [] = []
-- myMap f (x:xs) = (f x):(myMap f xs)

-- myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f [] = False
-- myAny f (x:xs) 
-- 	|f x = True
-- 	|otherwise = myAny f xs


-- myZip :: [a] -> [b] -> [(a,b)]
-- myZip _ [] = []
-- myZip [] _ = []
-- myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- bestFit :: Int -> Int 




recommended :: [Int] -> (Int -> Int) -> [(Int, Float)] -> [Int]
recommended [] _ _ = []
recommended _ _ [] = []
recommended basket bestFit products = (map (\product -> (bestFit product)) (filter (\x -> (findPrice (bestFit x)) < basketCost && (findPrice (bestFit x)) > 0 && (not (elem (bestFit x) basket))) basket)) 
 where price product = (filter (\x -> (fst x) == product) products)
       findPrice product= if (not (null (price product))) then (snd (head (price product))) else -1
       basketCost= (foldr (\curr res -> res + (findPrice curr)) 0 basket) 



--Song :: (String, String, Int)
--Songs :: [(String, String, Int)] -> Ascending by length 
--recommender :: (String, String, Int) -> String



rec :: [(String, String, Int)] -> ((String,String,Int) -> String)
rec pl = (\(comp, track, leng) ->
        let avgDuration comp = div (foldr (\(_, _, dur) res -> res + dur) 0  (filter (\(a, _, _) -> a == comp) pl)) (length (filter (\(a, _, _) -> a == comp) pl))
            option1 = (map (\(_, a, _) -> a ) (filter (\(comp1, track1, leng1)-> comp1 == comp && leng1 > leng) pl))
            option2 = (map (\(_, a, _) -> a ) (filter (\(comp1, track1, leng1) -> (avgDuration comp1) < (avgDuration comp)) pl))
	    in if not (null option1) then (head option1)
	       else if not (null option2) then (last option2)
           else (head ((map (\(_, a, _) -> a ) (filter (\(comp1, track1, leng1) -> leng1 > leng) pl) ++ [track]))))


addDefault:: a -> [a] -> [a]
addDefault val [] = [val]
addDefault val l = l

sumMinFix:: [(Float->Float)] -> [Float] -> Float
sumMinFix [] _ = 0
sumMinFix _ [] = 0
sumMinFix fx xl =  sum(map (\f -> minimum (addDefault 0 [x | x<-xl, (f x) == x])) fx)




filterByChar c ls = filter (\x -> (elem c x)) ls



merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = if x<y then x : (merge xs (y:ys)) else y : (merge (x:xs) ys)




test (x:y:z) = x:z

removeFirst:: (Eq a) => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) y = if x == y then xs else x:removeFirst xs y



-- permutate :: (Eq a) => [a] -> [[a]]
-- permutate [] = [[]]
-- permutate l = [a:x | a <- l, x <- (permutate $ removeFirst l a)]

perm :: Eq a => [a] -> [[a]]
perm [] = [[]]
perm xs = [y:tmp | y <- xs, tmp <- (perm (removeFirst xs y))]

-- [1:[2:[3:[[]]]]]

-- [1:[2:[[]]
-- [1:[2,3,[]]]
-- [1,2,3,[]]



-- factorial 0 = 1
-- factorial x 
--  | x < 0 = 0
--  | x > 0 = x*factorial(x-1)


totalMin:: [(Double->Double)] -> (Double->Double)
totalMin [] = (\x -> x)
totalMin xs = foldr1 (\curr res -> if (curr 0) < (res 0) then curr else res) xs


--chainMinCompositions:: (Num a) => (a -> a) -> Int -> (a -> a)
chainMinCompositions _ 0 = (\x -> x)
chainMinCompositions f 1 = (f)
chainMinCompositions f n = if first == second then (totalMin [(chainMinCompositions f val) | val<-[0..(n-1)]]) else ((chainMinCompositions f (n-1)).(chainMinCompositions f (n-2)))
 where first = [((chainMinCompositions f (n-1)) val)| val <- [0.0..n]]
       second = [((chainMinCompositions f (n-2)) val)| val <- [0.0..n]]







quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort lesser) ++ [x] ++ (quickSort greater)
 where lesser = filter (\y -> y <= x) xs
       greater = filter (\y -> y > x) xs


 
data People = Marto | Boci | Achkata
data Test = NotSmart | Average | Smart deriving (Show)

test2 :: People -> Test
test2 Marto = Smart
test2 _ = NotSmart

data BinTree = NilTree | Node Int BinTree BinTree deriving (Show)

sumTree NilTree = 0
sumTree (Node n t1 t2) = n + (sumTree t1) + (sumTree t2)



g([]:_) = []
g l = (map head l):(g (map tail l))