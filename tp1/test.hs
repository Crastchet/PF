--1
--somme n = sum [1..n]

--3
sommeDeXaY ::  Int -> Int -> Int
sommeDeXaY x y = 
	if x > y then
		0
	else
		x + sommeDeXaY (x+1) y
--4
somme :: [Int] -> Int
somme [] = 0
somme (x:xs) = x + somme xs

--5
-- last
last' :: [a] -> a
last' [] = error "Liste vide, je fais une exception alors que c'est pas bien, soit disant impur"
last' (_:x) = head (reverse x)

-- init
init' :: [a] -> [a]
init' [] = error "Liste vide, je fais une exception alors que c'est pas bien, soit disant impur"
init' (x:xs) = reverse (tail ((reverse (x:xs))))

--6
-- !!
index :: [a] -> Int -> a
index (x:xs) 0 = x
index (x:xs) i = index xs (i-1)
index [] _ = error "Liste vide"

-- ++
conkt :: [a] -> [a] -> [a]
conkt (x:xs) ys = x : conkt xs ys
conkt [] ys = ys

-- concat
concat' :: [[a]] -> [a]
concat' (x:xs) = conkt x (concat' xs)
concat' [] = []

-- map
map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = (f x) : (map' f xs)
map' _ [] = []

--7
-- La fonction x attend un paramètre p (de type Int) et renvoie l'élément (de type a) situé en position p dans la liste l

--8
longueurList :: [Int] -> Int
longueurList [] = 0
longueurList (xs) = somme (map (^0) (xs))


--9
-- Récursive
fonction :: (a -> a) -> a -> Int -> [a]
fonction _ x 0 = [x]
fonction f x n = x : fonction f ((f) x) (n-1)

-- Iterate / Take
fonction' :: (a -> a) -> a -> Int -> [a]
fonction' f x n = take (n+1) (iterate f x)

--10
entiersConsecutifs x = fonction ((+) 1) 0 x
