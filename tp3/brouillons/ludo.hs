interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config ((etatTortue:etatTortueSuite) , (path:pathSuite)) 'F' = let etatTortueBis = avance config etatTortue in ((etatTortueBis:etatTortueSuite), (path++[fst etatTortueBis]):pathSuite)
interpreteSymbole config ((etatTortue:etatTortueSuite) , (path:pathSuite)) '+' = let etatTortueBis = tourneAGauche config etatTortue in ((etatTortueBis:etatTortueSuite), (path++[fst etatTortueBis]):pathSuite)
interpreteSymbole config ((etatTortue:etatTortueSuite) , (path:pathSuite)) '-' = let etatTortueBis = tourneADroite config etatTortue in ((etatTortueBis:etatTortueSuite), (path++[fst etatTortueBis]):pathSuite)
interpreteSymbole config ((etatTortue:etatTortueSuite) , path) '[' = ((etatTortue:etatTortue:etatTortueSuite) , ([fst etatTortue]:path))
interpreteSymbole config ((etatTortue:etatTortueBis:etatTortueSuite) , path) ']' = ((etatTortueBis:etatTortueSuite) , ([fst etatTortueBis]:path))
interpreteSymbole config etatDessin _ = etatDessin

------------- Q9
interpreteMot :: Config -> Mot -> Picture
interpreteMot config mot = let i = etatInitial config in pictures (map line (interpreteMot' config (filtreSymbolesTortue config mot) ([i], [[fst i]])))

interpreteMot' :: Config -> Mot -> EtatDessin -> [Path]
interpreteMot' _ [] (etatTortue,path) = path
interpreteMot' config (x:xs) etatDessin = interpreteMot' config xs (interpreteSymbole config etatDessin x)