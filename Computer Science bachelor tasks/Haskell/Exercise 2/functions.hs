--Geting all Tuples two elements from a list
tuplesList::[Int] -> [Int] -> [(Int,Int)];
tuplesList xs ys=[(x,y)| x <- xs,y <- ys];
--Check if it is a accurate triangle 
accurateTri::[Int] -> [(Int,Int,Int)]
accurateTri xs=[(x,y,z)| x <- xs, y <- xs ,z <- xs ,x^2 + y^2 == z^2,x>y]