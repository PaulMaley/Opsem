module DataTypes where

{- Data types -}

-- Phrase (strange name)
data Phrase = CP Command
            | IP IntExp
            | BP BoolExp
            deriving (Show, Eq)

-- These are command expressions (complex)
data Command = Skip
             | Assign Ident IntExp
             | Seq Command Command
             | If BoolExp Command Command
             | While BoolExp Command
             deriving (Show, Eq)

data IntExp = IntVal Int
            | DeRef Ident
            | IntOpExp IntOp IntExp IntExp
            deriving (Show, Eq)

data BoolExp = BoolVal Bool
             | BoolOpExp BoolOp IntExp IntExp
             deriving (Show, Eq)

type Ident = String 

-- To complete ....
data IntOp = Add
           | Mult
           | Sub
           deriving (Show, Eq)

data BoolOp = Eq
            | Gt
            | Lt
            deriving (Show, Eq)

-- These are commands out of the context of an expression
data Control = CtlPhrase Phrase
             | CtlIOp IntOp
             | CtlBOp BoolOp
             | CtlAssign
             | CtlIf
--             | CtlSkip
             | CtlWhile
             deriving (Show, Eq)

data Result = ResPhrase Phrase
            | ResIdent Ident 
            deriving (Show, Eq)

{-
Imrpove on this .... interface
-}
{-
type Store = Map.Map Ident Int

emptyStore :: Store
emptyStore = Map.empty

extendStore :: Ident -> Int -> Store -> Store
extendStore id val s = Map.insert id val s

findInStore :: Ident -> Store -> Maybe Int
findInStore = Map.lookup  
-}

