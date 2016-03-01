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