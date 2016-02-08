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


type EtatDessin = (EtatTortue, Path)

--8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole c (et,p) 'F' = let et2 = avance c et in (et2, p++[(fst et2)])    
interpreteSymbole c (et,p) '+' = let et2 = tourneAGauche c et in (et2, p++[(fst et2)])
interpreteSymbole c (et,p) '-' = let et2 = tourneADroite c et in (et2, p++[(fst et2)])

--9
interpreteMot :: Config -> Mot -> Picture
interpreteMot c xs = let i = (etatInitial c) in Line (interpreteMot_rep c (i,[fst i]) (filtreSymbolesTortue c xs))
    
interpreteMot_rep :: Config -> EtatDessin -> Mot -> Path
interpreteMot_rep _ (et,p) [] = p
interpreteMot_rep c ed (x:xs) = interpreteMot_rep c (interpreteSymbole c ed x) xs

--10
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime ls c instant = let i = round instant `mod` 6 in interpreteMot c (ls !! i)

------------------------------- TESTS -------------------------------
vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

---------------------------------------------------------------------

main = animate (InWindow "Tortue" (1000, 1000) (0, 0)) white vonKoch1Anime