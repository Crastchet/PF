import Graphics.Gloss
------------------------

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
vonKoch :: Regles
vonKoch ' ' = " "
vonKoch 'F' = "F-F++F-F"
vonKoch '+' = "+"
vonKoch '-' = "-"

--3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme [] _ = []
lsysteme xs r = motSuivant r xs : lsysteme (motSuivant r xs) r


-------------
--- TORTUE---
-------------

type EtatTortue = (Point, Float)

type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

--4
etatInitial :: Config -> EtatTortue
etatInitial (ei,li,fe,ar,ls) = ei

longueurPas :: Config -> Float
longueurPas (ei,li,fe,ar,ls) = li

facteurEchelle :: Config -> Float
facteurEchelle (ei,li,fe,ar,ls) = fe

angle :: Config -> Float
angle (ei,li,fe,ar,ls) = ar

symbolesTortue :: Config -> [Symbole]
symbolesTortue (ei,li,fe,ar,ls) = ls

--5
avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y),cap) = ((x + (longueurPas c) * cos(cap) , y + (longueurPas c) * sin(cap)) , cap)

--6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c ((x,y),cap) = ((x,y) , cap + angle c)

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite c ((x,y),cap) = ((x,y) , cap - angle c)

--7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue c [] = []
filtreSymbolesTortue c (x:xs) = if elem x (symbolesTortue c) then
	x : filtreSymbolesTortue c xs
else
	filtreSymbolesTortue c xs


type EtatDessin = ([EtatTortue], [Path])

--8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin -- On ajoute la dernière position de la tortue à la liste des points Path, mais pas a nouvelle. On le fera dans interpreterMot_rep
interpreteSymbole c ((pos,cap):ets,ps:pss) 'F' = ((avance c (pos,cap)):ets,((pos:ps):pss))
interpreteSymbole c ((pos,cap):ets,ps:pss) '+' = ((tourneAGauche c (pos,cap)):ets,((pos:ps):pss))
interpreteSymbole c ((pos,cap):ets,ps:pss) '-' = ((tourneADroite c (pos,cap)):ets,((pos:ps):pss))
interpreteSymbole c ((et:ets),ps) '[' = ((et:et:ets),[fst et]:ps)
interpreteSymbole c ((et1:et2:ets),ps) ']' = (et2:ets,[fst et2]:ps)
interpreteSymbole _ _ _ = error

--9
interpreteMot :: Config -> Mot -> Picture
interpreteMot c (xs) = line (interpreteMot_rep c ((etatInitial c):[],[]) (filtreSymbolesTortue c xs)) 

interpreteMot_rep :: Config -> EtatDessin -> Mot -> Path
interpreteMot_rep c ((pos,cap):ets,ps:pss) [] = (pos:ps) -- C'est ici que j'ajoute la position courante de la tortue car elle ne s'ajoute pas à la liste des points Path dans EtatDessin
interpreteMot_rep c ed (x:xs) = interpreteMot_rep c (interpreteSymbole c ed x) xs

--10
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime ls c instant = 
  let i = round instant `mod` 15 in interpreteMot c (ls !! i)

------------------------------- TESTS -------------------------------
brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]

brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")

---------------------------------------------------------------------

main = animate (InWindow "Tortue Volante" (1000, 1000) (0, 0)) white brindilleAnime