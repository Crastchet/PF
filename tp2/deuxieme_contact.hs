-------------------------------------------------------------------------------
import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))
-------------------------------------------------------------------------------

--1
alterne :: [a] -> [a]
alterne (x:[]) = x : []
alterne (x:xs) = x : alterne (tail xs)
alterne [] = []

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

--5
type Point = (Float,Float)
type Path = [Point]

pointAintercaler :: Point -> Point -> Point
pointAintercaler (ax,ay) (bx,by) = ((ax+bx)/2 + (by-ay)/2, (ay+by)/2 + (bx-ax)/2)

pasDragoon :: Path -> Path
pasDragoon (x:[]) = x : []
pasDragoon (x:xs) = x : pointAintercaler x (head xs) : pasDragoon xs

dragon :: Point -> Point -> [Path]
dragon x y = iterate pasDragoon [x,y]