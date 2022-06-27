import Data.List hiding (groupBy, minimumBy)

data Point = Point {
  pX :: Double,
  pY :: Double
} deriving (Show)

-- изберете си между това и горното представяне на точка
-- data Point = Point Double Double deriving Show

instance Eq Point where
-- TODO:
	(Point x y) == (Point z h) = (x==z)&&(y==h)

data Cluster = Cluster {
  cId :: Int,
  cCenter :: Point
} deriving (Show)

-- същото като за точка
-- data Cluster = Cluster Int Point deriving Show

instance Eq Cluster where
-- TODO:
	(Cluster pt1 id1) == (Cluster pt2 id2) = (pt1==pt2)&&(id1==id2);



-- centroid :: [Point] -> Maybe Point
-- сумата на всички точки / броят им
-- TODO:
centroid :: [Point] -> Maybe Point
centroid [] = Nothing
centroid xs = Just (divPoint (foldl sumPoints (Point 0 0) xs) (length xs)) 
	where 
		sumPoints (Point x y) (Point z h) = Point (x+z) (y+h)
		divPoint (Point x y) size = Point (x / (fromIntegral size)) (y / (fromIntegral size)) 	

-- sqDistance :: Point -> Point -> Double
-- квадрат на евклидово разстояние
-- TODO:
sqDistance :: Point -> Point -> Double
sqDistance (Point x y) (Point z h) = ((z-x)^2) + ((h-y)^2)


-- splitBy :: (Eq a) => a -> [a] -> [[a]]
-- може да видите как се използва долу в main
-- TODO:
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy el xs =[takeWhile (\x -> x /= el) xs ,dropWhile (\x -> x /= el) xs] 


-- nearest :: [Cluster] -> Point -> Maybe Cluster
-- искаме да проверим кой е най-близкият cluster за дадена точка
-- т.е. центърът на кой cluster е най-близо до нашата точка.
-- TODO:
nearest :: [Cluster] -> Point -> Maybe Cluster
nearest [] _ = Nothing
nearest (startCluster:clList) pt = Just (foldr minDistance  startCluster clList)
	where
	minDistance currClust accCluster = if (sqDistance (cCenter currClust) pt)<(sqDistance (cCenter accCluster) pt) then currClust else accCluster

-- partition:: (a -> a -> Bool) -> [a] -> [[a]]
-- групираме елементи на списък според релация на еквивалентност, която ни дават отвън
-- т.е. правим разбиване на класове на еквивалентност
-- например : (partition (\x y -> x == y) [1,2,1,2,1,2,1]) -> [[1,1,1,1],[2,2,2]]
-- TODO:
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

partition1::(Ord a) => (a -> a -> Bool) -> [a] -> [[a]]
partition1 _ [] = []
partition1 f lst = [(filter (\y -> f x y) lst) | x <- (removeDuplicates lst)] 


main :: IO ()
main = do
pointsFile <- readFile "C:\\Users\\AZ\\Desktop\\Computer Science year 2\\Functional Programming\\Haskell\\HaskellHomework\\points.txt"
let pointsLines = lines pointsFile
let points = map (\line -> case splitBy ' ' line of [x,y] -> Point (read x :: Double) (read y :: Double)) pointsLines  
mapM_ (putStrLn . show) points 
 