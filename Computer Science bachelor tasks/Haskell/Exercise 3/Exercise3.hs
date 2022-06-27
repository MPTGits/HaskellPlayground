quadrant::(Int,Int) -> Int
quadrant (x,y)
	| x>0 && y>0 = 1
	| x<0 && y>0 = 2
	| x<0 && y<0 = 3
	| x>0 && y<0 = 4
	
-- append 2 lists toghether
append1 :: [a] -> [a] -> [a]
append1 [] y = y
append1 (x:xs) y =  x : append1 xs y
-- append [1, 2, 3] [5, 6, 7] -> [1, 2, 3, 5, 6, 7]
-- 
-- remove the brackets from the inner lists
myFlatten :: [[a]] -> [a]
myFlatten [] = []
myFlatten (x:xs) = x ++ myFlatten xs 
-- myFlatten [[1, 2, 3], [4, 5, 6]] -> [1, 2, 3, 4, 5, 6]
-- 
-- returns a list of the first n elements
myTake :: Int -> [a] -> [a] 
myTake 0 xs = []
myTake n (x:xs) = x : myTake (n-1) xs 
-- myTake 3 [1, 2, 3, 4, 5] -> [1, 2, 3]
-- 
-- returns a list from the elements between the 2 given indexes
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs x y =  (myTake (y - x) (drop x xs)) 
-- mySlice [1, 2, 3, 4, 5, 6, 7] 2 4 -> [3, 4, 5]

-- counts how many times the elements is found in the list
countOccurances :: (Eq a) => [a] -> a -> Int
countOccurances [] y = 0
countOccurances (x:xs) y = if x == y then 1 + countOccurances xs y else countOccurances xs y 
-- countOccurances [1, 2, 4, 5, 6, 5, 6] 5 -> 2
-- 
-- removes all occurances of the given element from the list
remove :: (Eq a) => [a] -> a -> [a]
remove [] y = []
remove (x:xs) y = if x == y then remove xs y else x : remove xs y 
--Filter solution 
remove2 :: (Eq a) => [a] -> a -> [a]
remove2 xs y = filter (/=y) xs
-- remove [1, 3, 5, 6, 3, 5, 3] 3 -> [1, 5, 6, 5] 
-- 
-- checks if the digits in the number are in descending order
isDescending :: Int -> Bool
isDescending x 
	| x < 10 = True
	| (x `mod` 10) >= ((x `div` 10) `mod` 10) = isDescending (x `div` 10)
	| otherwise = False  
	
-- isDescending 55544332222 -> True
-- 
-- find the divisors of the given number
divisors :: Int -> [Int]
divisors x = filter (\y -> (x `mod` y) == 0) [1..x]  

divisors' n = [y | y <- [1..n], n `mod` y == 0]
-- divisors 15 -> [1, 3, 5, 15]
-- 
-- check if the given number is prime using one of the previous tasks
prime :: Int -> Bool
prime x = if (length (divisors x) == 2) then True else False 	
-- prime 7 -> True
-- prime 11 -> False
-- 
-- check if the first number ends with the second one
endsWith :: Int -> Int -> Bool
endsWith firstNum secondNum 
	| secondNum<10 = num1Rem == secondNum
	| num1Rem /= num2Rem = False 
	| otherwise = endsWith (fistNum `mod` 10) (secondNum `mod` 10)
		where 	num1Rem = firsNum `rem` 10
				num2Rem = secondNum `rem` 10
-- endsWith 145 45 -> True
-- endsWith 1456 5 -> false