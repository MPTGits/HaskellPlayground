a::Int 
a=7;

next::Int->Int
next a=a+1

sum2::Int->Int->Int 
sum2 a b = a  + b 


sum3::Int->Int->Int->Int
sum3 a b c = a + b + c

fact:: Integer -> Integer 
fact 0 =1
fact n = n * fact(n - 1)

fibo:: Integer -> Integer 
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n - 1) + fibo(n - 2)

max1:: Int -> Int -> Int
max1 x y = if x > y then x else y

lenOfList:: [Int] -> Int
lenOfList xs = if null xs then 0 else 1 + lenOfList (tail xs) 

myReverse:: [Int] -> [Int]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

maxElem :: [Int] -> Int
maxElem [x] = x
maxElem (x : xs) = if x > maxElem xs then x else maxElem xs  