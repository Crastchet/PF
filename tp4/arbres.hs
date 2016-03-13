import Test.QuickCheck
import Control.Concurrent (threadDelay)
---------------------------------------------------

--1
data Arbre coul val =
	Feuille
	| Noeud coul val (Arbre coul val) (Arbre coul val)
	deriving Show

data Couleur = Rouge | Noir
	deriving Show


--2
map' :: (coul -> a -> b) -> Arbre coul a -> Arbre coul b
map' _ Feuille = Feuille
map' f (Noeud c v g d) = Noeud c (f c v) (map' f g) (map' f d)

fold :: (coul -> a -> b -> b -> b) -> b -> (Arbre coul a) -> b
fold _ x0 Feuille = x0
fold f x0 (Noeud c v g d) = f c v (fold f x0 g) (fold f x0 d)


--3
hauteur :: Arbre coul val -> Int -- Faut il prendre en compte le premier noeud et les feuilles ?
hauteur Feuille = 0
hauteur (Noeud c v g d) = (max (hauteur g) (hauteur d)) + 1

taille :: Arbre coul val -> Int -- Faut il prendre en compte le premier noeud (pour l'instant la fonction le fait)
taille Feuille = 0
taille (Noeud c v g d) = taille g + taille d + 1

hauteur' :: Arbre coul val -> Int
hauteur' arb = fold (\_ _ g d -> (max g d) + 1) 0 arb

taille' :: Arbre coul val -> Int
taille' arb = fold (\_ _ g d -> g + d + 1) 0 arb


--4
peigneGauche :: [(coul,val)] -> Arbre coul val
peigneGauche [] = Feuille
peigneGauche ((c,v):cvs) = Noeud c v (peigneGauche (cvs)) Feuille


--5
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)
prop_hauteurPeigne' xs = length xs == hauteur' (peigneGauche xs)
-- Elle vérifie que le peigne a été créé correctement à partir de la liste xs (en supposant que la fonction hauteur soit bonne). Vu que le peigne est un arbre qu'avec des sous arbres côté gauche, la taille de la liste et la hauteur de l'arbre doivent être pareils


--6
prop_taillePeigne xs = length xs == taille (peigneGauche xs)
prop_taillePeigne' xs = length xs == taille' (peigneGauche xs)


--7
estComplet :: Arbre coul val -> Bool
estComplet Feuille = True
estComplet (Noeud _ _ g d) = estComplet g && estComplet d && (hauteur g == hauteur d)


--8
-- Peigne à gauche complet = arbre vide


--9
complet :: Int -> [(c, a)] -> Arbre c a
complet 0 _ = Feuille
complet _ [] = error "Liste de couleurs valeurs vide"
complet h xs = Noeud c v (complet (h-1) fg) (complet (h-1) fd)
         where (fg, ((c,v):fd)) = splitAt (length xs `quot` 2) xs


--10
-- Fonction = repeat
repeat' :: a -> [a]
repeat' = iterate id


--11
creerListe = [((),u) | u <- ['a'..]]


--12
aplatit :: Arbre coul val -> [(coul,val)]
aplatit Feuille = []
aplatit (Noeud c v g d) = aplatit g ++ [(c,v)] ++ aplatit d

prop_arbreAplatit complet4 = map snd (aplatit complet4) == [('b',1),('b',2),('b',3),('b',4),('b',4),('b',3),('b',4),('b',4),('b',2),('b',3),('b',4),('b',4),('b',3),('b',4),('b',4)]
-- à revoir car au test quickCheck ça ne passe pas !


--13
element :: Eq val => val -> Arbre coul val -> Bool
element _ Feuille = False
element x (Noeud _ v g d) = (x == v) || (element x g) || (element x d)


--14
noeud :: (coul -> String) -> (val -> String) -> (coul,val) -> String
noeud fc fv (c,v) = fv v ++ " [color=" ++ (fc c) ++ ", fontcolor=" ++ fc c ++ "]"


--15
getValeur :: Arbre coul val -> val
getValeur Feuille = error "Pas de valeur"
getValeur (Noeud _ v _ _) = v

arcs :: Arbre c val -> [(val,val)]
arcs Feuille = []
arcs (Noeud _ _ Feuille Feuille) = [] -- obligé à cause de getValeur, on pourrait modifier getValeur pour que quand il reçoit une Feuille, il renvoie la valeur de son père (passée en paramètre de la fonction). Ainsi on aurait un arc d'un noeud qui pointe vers lui même, mais je ne pense pas que ce soit une bonne alternative
arcs (Noeud _ v g Feuille) = [(v, getValeur g)] ++ (arcs g) -- pareil
arcs (Noeud _ v Feuille d) = [(v, getValeur d)] ++ (arcs d) -- pareil
arcs (Noeud _ v g d) = [(v,getValeur g) , (v,getValeur d)] ++ (arcs g) ++ (arcs d)


--16
arc :: (val -> String) -> (val, val) -> String
arc f (v1,v2) = (f v1) ++ " -> " ++ (f v2)


--17
dotise :: String -> (coul -> String) -> (val -> String) -> Arbre coul val -> String
dotise n fc fv arb = unlines (
	["digraph \"" ++ n ++ "\" { node [fontname=\"DejaVu-Sans\", shape=circle]"]
	++ (map (noeud fc fv) (aplatit arb))
	++ (map (arc fv) (arcs arb))
	++ ["}"]
	)

--18
elementR :: (Eq val, Ord val) => val -> Arbre coul val -> Bool
elementR _ Feuille = False
elementR x (Noeud _ v g d)  | x == v = True
                            | x < v  = elementR x g
                            | x > v  = elementR x d


--19
-- Déjà fait question 1 car vu en salle TP au tableau


--20
equilibre :: Arbre Couleur val -> Arbre Couleur val
equilibre Feuille = Feuille
equilibre (Noeud _ z (Noeud Rouge y (Noeud Rouge x a b) c) d) = Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d)
equilibre (Noeud _ z (Noeud Rouge x a (Noeud Rouge y b c)) d) = Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d)
equilibre (Noeud _ x a (Noeud Rouge z (Noeud Rouge y b c) d)) = Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d)
equilibre (Noeud _ x a (Noeud Rouge y b (Noeud Rouge z c d))) = Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d)
equilibre arb = arb


--21
addVal :: (Eq val, Ord val) => val -> Arbre Couleur val -> Arbre Couleur val
addVal x Feuille = (Noeud Noir x Feuille Feuille) -- Noir car noeud racine
addVal x (Noeud c v Feuille d) | x < v = (Noeud c v (Noeud Rouge x Feuille Feuille) d) -- Rouge car ajour sur sous-arbre vide
addVal x (Noeud c v g Feuille) | x > v = (Noeud c v g (Noeud Rouge x Feuille Feuille)) -- Idem
addVal x (Noeud c v g d)	| x < v = equilibre (Noeud c v (addVal x g) d)
							| x > v = equilibre (Noeud c v g (addVal x d))
							| x == v = (Noeud c v g d)


--22
-- Après aplatissement, jamais plus de 3 Noir consécutifs, jamais plus de 2 Rouge consécutifs.


--23
arbreSuivant :: (Eq val, Ord val) => [val] -> Arbre Couleur val -> [Arbre Couleur val]
arbreSuivant [] arb = [arb]
arbreSuivant (v:vs) arb = arb : (arbreSuivant vs (addVal v arb))

stringFromColor :: Couleur -> String
stringFromColor Rouge = "red"
stringFromColor Noir = "black"

stringFromValue = \x -> [x]

arbresDot :: String -> [String] -- contraint de changer la signature car le compilateur sait qu'il attend un [Char], et ne veut pas accepter le [a]
arbresDot vs = map (dotise "Mon arbre animé" stringFromColor stringFromValue) (arbreSuivant vs Feuille) -- on démarre d'une feuille


------------------------------------------------------------------------------


main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"
------------------------------------------------------------------------------



------ tests persos ------

mytree = Noeud "b" 1 
			(Noeud "b" 2 
				(Noeud "b" 3 Feuille Feuille)
				Feuille)
			(Noeud "b" 2
				Feuille
				(Noeud "b" 3
					(Noeud "b" 4
						Feuille
						Feuille)
					Feuille))

complet4 = Noeud "b" 1 
			(Noeud "b" 2 
				(Noeud "b" 3
					(Noeud "b" 4 Feuille Feuille)
					(Noeud "b" 4 Feuille Feuille))
				(Noeud "b" 3
					(Noeud "b" 4 Feuille Feuille)
					(Noeud "b" 4 Feuille Feuille)))
			(Noeud "b" 2 
				(Noeud "b" 3
					(Noeud "b" 4 Feuille Feuille)
					(Noeud "b" 4 Feuille Feuille))
				(Noeud "b" 3
					(Noeud "b" 4 Feuille Feuille)
					(Noeud "b" 4 Feuille Feuille)))