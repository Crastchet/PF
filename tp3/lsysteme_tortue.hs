type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

--1
motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (x:xs) = (r x) ++ motSuivant r xs

--motSuivant'' :: Regles -> Mot -> Mot
--motSuivant'' r (x:xs) = take 1 (iterate (r x))

--2
vonKoch 'F' = F − F + +F − F
vonKoch '+' = '+'
vonKoch '-' = '-'

