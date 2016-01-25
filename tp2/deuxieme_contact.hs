--1
alterne :: [a] -> [a]
alterne [] = []
alterne (x:[]) = x : []
alterne (x:xs) = x : alterne (tail xs)

--2
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine _ [] _ = []
combine _ _ [] = []
combine f (x:xs) (y:ys) = (f x y) : combine f xs ys

--3
pasPascal :: [Int] -> [Int]
pasPascal (x:xs) = 1 : (zipWith (+) (x:xs) (xs)) ++ [1]

--4
pascal :: [[Int]]
pascal = iterate pasPascal [1]
