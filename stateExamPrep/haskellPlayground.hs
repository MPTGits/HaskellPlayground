//Haskell playground

fibonaci:: Int -> Int -> Int
fibonaci 0 = 0
fibonaci 1 = 1
fibonaci x y = fibonaci(x-1) + fibonaci(x-2)