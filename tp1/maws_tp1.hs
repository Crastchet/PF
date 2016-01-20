-- Exerice 3 : sommeDeXaY
sommeDeXaY::Int -> Int -> Int
sommeDeXaY x y =
    if(x > y) then
        0
    else
        x + sommeDeXaY (x+1) y

-- Exercice 4 : somme
somme::[Int]->Int
somme [] = 0;
somme (x:xs) = x + somme xs

-- Exercice 5.1 : last
last'::[a]->a
last' [] = error "List vide !"
last' xs = xs !! (length xs - 1)

-- Exercice 5.2 : init
init'::[a]->[a]
init' [] = error "List vide !"
init' xs = take (length xs - 1) xs

--Exercice 6.1 : !!
getN::[a]->Int->a
getN [] _ = error "Liste vide !"
getN (x:_) 0 = x
getN (x:xs) n = getN xs (n-1)

--Exercice 6.2 : ++
add::[a]->[a]->[a]
add [] [] = []
add xs [] = xs
add [] ys = ys
add (x:xs) (ys) = x : add xs (ys)

--Exercice 6.3 : concat
concat'::[[a]]->[a]
concat' [[]] = []
concat' [] = []
concat' (x:xs) = add x (concat' xs)

--Exercice 6.4 : map
map':: (a->b) -> [a] -> [b]
map' (_) [] = []
map' f (x:xs) = (f x) : map' f xs

--Exercice 7
-- On déclare une fonction x qui attend un argument de type Int et qui renvoi un element de la liste de type a

--Exercice 8
longueurList::[Int] -> Int
longueurList [] = 0
longueurList (xs) = somme (map' ((^) 1) xs)

--Exercice 9 
--Version récursive--
apply :: (a->a) -> a -> Int -> [a]
apply _ x 0 = [x]
apply f x n = x : apply f ((f) x) (n-1)

--Version avec iterate et take--
apply2::(a->a) -> a -> Int -> [a]
apply2 f x n = take (n + 1) (iterate f x)

--Exercice 10
entierConsecutif n =
    apply ((+) 1) 0 n