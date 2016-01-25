import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))

--Exercice 5
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA, yA) (xB, yB) = (( xA + xB ) / 2 + ( yB - yA ) / 2 , ( yA + yB ) / 2 + ( xA - xB ) / 2)

dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre a b 0 = [a,b]
dragonOrdre a b n = 
    (dragonOrdre a (pointAintercaler a b) (n-1)) ++ (dragonOrdre b (pointAintercaler a b) (n-1))