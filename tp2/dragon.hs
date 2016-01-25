-------------------------------------------------------------------------------
import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))
-------------------------------------------------------------------------------

--5
pointAintercaler :: Point -> Point -> Point
pointAintercaler (ax,ay) (bx,by) = ((ax+bx)/2 + (by-ay)/2, (ay+by)/2 + (ax-bx)/2)

--6
pasDragoon :: Path -> Path
pasDragoon (x:[]) = x : []
pasDragoon [x,y] = [x, (pointAintercaler x y), y]
pasDragoon (x:y:z:xs) = x : (pointAintercaler x y) : y : (pointAintercaler z y) : pasDragoon (z:xs)

--7
dragon :: Point -> Point -> [Path]
dragon x y = iterate pasDragoon [x,y]
