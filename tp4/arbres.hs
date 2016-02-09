data Arbre coul val =
	Feuille
	| Noeud coul val (Arbre coul val) (Arbre coul val)
	deriving Show

data Couleur = Rouge | Noir

--1
map' :: (coul -> a -> b) -> Arbre coul a -> Arbre coul b
map' f Feuille = Feuille
map' f (Noeud c v g d) = Noeud c (f c v) (map' f g) (map' f d)

fold :: (coul -> a -> b -> b -> b) -> b -> (Arbre coul a) -> b
fold f v 