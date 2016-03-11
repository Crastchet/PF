import Test.QuickCheck
---------------------------------------------------

--1
data Arbre coul val =
	Feuille
	| Noeud coul val (Arbre coul val) (Arbre coul val)
	deriving Show

data Couleur = Rouge | Noir


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

myTreeComplet = Noeud "b" 1 
			(Noeud "b" 2 
				(Noeud "b" 3 Feuille Feuille)
				(Noeud "b" 3 Feuille Feuille))
			(Noeud "b" 2 
				(Noeud "b" 3 Feuille Feuille)
				(Noeud "b" 3 Feuille Feuille))