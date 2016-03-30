module Interprete where
import Parser
import Data.Char
import Data.Maybe

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show, Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show, Eq)

-- Executer un Parser : runParser
-- Facilite les tests pour les exemples
parse :: Parser a -> String -> Resultat a
parse = runParser


--1
espacesP :: Parser ()
espacesP = do zeroOuPlus (car ' ')
              return ()

--2
verifierLettre :: Parser Char
verifierLettre = carCond (`elem` ['a'..'z'])

nomP :: Parser Nom
nomP = do cs <- unOuPlus verifierLettre
          espacesP
          return cs

--3
varP :: Parser Expression
varP = do x <- nomP
          return (Var x)

--4
applique :: [Expression] -> Expression
applique = foldl1 App

--5
exprP :: Parser Expression
exprP = varP ||| exprParentheseeP ||| lambdaP ||| nombreP ||| booleenP

exprsP :: Parser Expression
exprsP = do exprs <- unOuPlus exprP
            return (applique exprs)

--6
lambdaP :: Parser Expression
lambdaP = do car '\\'
             espacesP
             x <- varP
             espacesP
             car '-'
             car '>'
             espacesP
             expr <- exprsP
             return (Lam (get x) expr)
  where get (Var xx) = xx
        get _ = error ""

--8
exprParentheseeP :: Parser Expression
exprParentheseeP = do car '('
                      e <- exprsP
                      car ')'
                      espacesP
                      return e

--9
chiffre :: Parser Char
chiffre = carCond isDigit

nombre :: Parser String
nombre = unOuPlus chiffre

entier :: Parser Integer
entier = do nb <- nombre
            return (read nb)

nombreP :: Parser Expression
nombreP = do e <- entier
             espacesP
             return (Lit (Entier e))

--10
booleenP :: Parser Expression
booleenP = do b <- chaine "True" ||| chaine "False"
              return (Lit (Bool (b == "True")))

--11
expressionP :: Parser Expression
expressionP = do espacesP
                 exprsP

--12
ras :: String -> Expression
ras s = case result of
        Just (r, "") -> r
        _ -> error "Erreur d’analyse syntaxique"
        where result = runParser expressionP s


--13
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

instance Show ValeurA where
   show (VFonctionA _)          = "lambda "
                      -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                      --   autre représentation des fonctions
   show (VLitteralA (Entier n)) = show n
   show (VLitteralA (Bool n))   = show n

--15
type Environnement a = [(Nom, a)]

interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _ (Lit l)      = VLitteralA l
interpreteA env (Lam n e)  = VFonctionA (\v -> interpreteA ((n, v):env) e)
interpreteA env (Var x)    = fromJust (lookup x env)
interpreteA env (App e e') = f (interpreteA env e')
    where f = case interpreteA env e of (VLitteralA _) -> error ""
                                        (VFonctionA r) -> r


--16
negA :: ValeurA
negA = (VFonctionA (\(VLitteralA (Entier a)) -> (VLitteralA (Entier (negate a)))))

-- Etape 17 (Avec defi notation sans fct exterieur)
addA :: ValeurA
addA = VFonctionA (\x1 -> case x1 of
                           (VLitteralA (Entier n1)) -> VFonctionA (\x2 -> case x2 of
                                                                           (VLitteralA (Entier n2)) -> VLitteralA (Entier (n1+n2))
                                                                           _                        -> undefined)
                           _                        -> undefined)


-- 18
envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot) 
       , ("if",    ifthenelseA)]

releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA op = (VFonctionA (\(VLitteralA (Entier a)) -> VFonctionA (\(VLitteralA (Entier b)) -> (VLitteralA (Entier (a `op` b))))))


-- 19
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool b)) -> (VFonctionA (\(VLitteralA (Entier t)) -> (VFonctionA (\(VLitteralA (Entier e)) ->
  case b of
    True -> (VLitteralA (Entier t))
    False -> (VLitteralA (Entier e)))))))


--20
---------------------------------------------------------------------------------
main::IO()
main = do putStr "philanthrope> "
          expr <- getLine
          putStrLn expr
          putStrLn (" -> " ++ show (interpreteA envA (ras expr)))
          main
---------------------------------------------------------------------------------


--21
data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB


instance Show ValeurB where
    show (VFonctionB _) = show '^'
    show (VLitteralB (Entier a)) = show a
    show (VLitteralB (Bool b)) = show b


--22
interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _ (Lit a) = Right (VLitteralB a)
interpreteB envi (Var a)| isJust (lookup a envi)= Right (fromJust (lookup a envi))
interpreteB _ (Var a) = Left ("la variable " ++ a ++ " n'est pas definie")
interpreteB envi (Lam nom expr) = Right (VFonctionB (\var -> interpreteB ((nom,var):envi) expr))
interpreteB envi (App e e') = case interpreteB envi e of
                                Left a -> Left a
                                Right (VFonctionB f)-> case interpreteB envi e' of
                                                            Left a -> Left a
                                                            Right a -> f a
                                Right v -> Left (show v ++ " n'est pas une fonction, application impossible.")
                                
-- Question 23
envB :: Environnement ValeurB
envB = [("add",   addB)
       , ("quot", quotB)]

addB :: ValeurB
addB = VFonctionB (\param -> case param of
                                VLitteralB (Entier e) -> Right (VFonctionB (\param' -> case param' of                                
                                                                                                VLitteralB (Entier e') -> Right (VLitteralB (Entier (e+e')))
                                                                                                x' -> Left (show x' ++ " n'est pas un entier.")))
                                x -> Left (show x ++ " n'est pas un entier."))


--24
quotB :: ValeurB
quotB = VFonctionB (\param -> case param of
                                VLitteralB (Entier e) -> Right (VFonctionB (\param' -> case param' of       
                                                                                                VLitteralB (Entier 0) -> Left "division par zero"                         
                                                                                                VLitteralB (Entier e') -> Right (VLitteralB (Entier (quot e e')))
                                                                                                x' -> Left (show x' ++ " n'est pas un entier.")))
                                x -> Left (show x ++ " n'est pas un entier."))
