import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

--Exercice 5
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA, yA) (xB, yB) = (( xA + xB ) / 2 + ( yB - yA ) / 2 , ( yA + yB ) / 2 + ( xA - xB ) / 2)

--Exercice 6
pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [x] = [x]
pasDragon [x,y] = [x, (pointAintercaler x y) ,y]
pasDragon (x:y:z:xs) = x : (pointAintercaler x y) : y : (pointAintercaler z y) : pasDragon (z:xs)

--Exercice 7
dragon :: Point -> Point -> [Path]
dragon a b = iterate pasDragon [a, b]
