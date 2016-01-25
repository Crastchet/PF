-------------------------------------------------------------------------------
import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))
-------------------------------------------------------------------------------

pointAintercaler :: Point -> Point -> Point
pointAintercaler (ax,ay) (bx,by) = ((ax+bx)/2 + (by-ay)/2, (ay+by)/2 + (ax-bx)/2)

-------------------------------------------------------------------------------

--8
dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre a b 0 = [a,b]
dragonOrdre a b n = (dragonOrdre a (pointAintercaler a b) (n-1)) ++ (dragonOrdre b (pointAintercaler a b) (n-1))
