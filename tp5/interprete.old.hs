import Parser
----------------------------------------------------------------

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)


--1
espacesP :: Parser ()
espacesP = do zeroOuPlus (car ' ')
              return ()

--2
nomP :: Parser Nom
