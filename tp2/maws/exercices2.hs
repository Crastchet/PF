import Graphics.Gloss.Data.Point

--Exercice 1
alterne::[a] -> [a]
alterne [] = []
alterne (x:xs) = x : alterne((drop 1 xs))

--Exercice 2
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine _ _ [] = []
combine _ [] _ = []
combine f (x:xs) (y:ys) = ((f) x y) : combine f xs ys


--Exercice 3
pasPascal :: [Integer] -> [Integer]
pasPascal (x:xs) = 1 : (zipWith (+) (x:xs) xs) ++ [1]

--Exercice 4
pascal :: [[Integer]]
pascal = iterate pasPascal [1]