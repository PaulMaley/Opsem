module Opsem where

import qualified Data.Map as Map
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
--import Control.Monad.Identity
import Data.Functor.Identity

-- REDESIGN THE FUCKING TYPES !!! THEY ARE SHIT
-- Possible to do while remaining faithful to the course ??
-- (Deriving Eq) is for unit testing

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
--             | CtlSkip
             | CtlWhile
             deriving (Show, Eq)

data Result = ResPhrase Phrase
            | ResIdent Ident 
            deriving (Show, Eq)

{-
Imrpove on this .... interface
-}

type Store = Map.Map Ident Int
emptyStore :: Store
emptyStore = Map.empty

extendStore :: Ident -> Int -> Store -> Store
extendStore id val s = Map.insert id val s

findInStore :: Ident -> Store -> Maybe Int
findInStore = Map.lookup  

findInStoreM :: Ident -> (ExceptT String) (State SMC) Int
findInStoreM id = ExceptT (do (_,_,s) <- get
                              case (findInStore id s) of
                                Nothing -> return (Left ("Identifier "++id++" not in Store")) 
                                Just n -> return (Right n))


{- 
Machine state: A triple:
  ControlStack :: [Control]
  ResultStack ::  [Result]
  Store :: Ident -> IntVal
-}

type SMC = ([Control],[Result],Store)  

type SMCm a = (ExceptT String) (State SMC) a


load :: Phrase -> Store -> SMC
load p s = ([CtlPhrase p], [], s)


stepM :: (ExceptT String) (State SMC) ()  
stepM = do
          c <- popCtlM
          case c of 
            {- Int expressions -}
            (CtlPhrase (IP exp)) -> case exp of
               (IntVal n) -> pushResM (ResPhrase (IP (IntVal n)))
               (DeRef id) -> do 
                               n <- findInStoreM id 
                               pushResM (ResPhrase (IP (IntVal n)))

               (IntOpExp iop ie1 ie2) -> do pushCtlM (CtlIOp  iop)
                                            pushCtlM (CtlPhrase (IP (ie2)))
                                            pushCtlM (CtlPhrase (IP (ie1)))
            (CtlIOp op) -> do
                             r2 <- popResM
                             r1 <- popResM
                             case (r2,r1) of
                               (ResPhrase (IP (IntVal n2)),ResPhrase (IP (IntVal n1))) ->
                                 case op of   
                                   Add -> pushResM (ResPhrase (IP (IntVal (n1 + n2))))
                                   Mult -> pushResM (ResPhrase (IP (IntVal (n1 * n2))))
                                   Sub -> pushResM (ResPhrase (IP (IntVal (n1 - n2))))
                               otherwise -> ExceptT (return (Left "IOp: Malformed stack")) 

            {- Bool expressions -}
            (CtlPhrase (BP exp)) -> case exp of 
              (BoolVal b) -> pushResM (ResPhrase (BP (BoolVal b)))
              (BoolOpExp bop ie1 ie2) -> do pushCtlM (CtlBOp  bop)
                                            pushCtlM (CtlPhrase (IP (ie2)))
                                            pushCtlM (CtlPhrase (IP (ie1)))
            (CtlBOp op) -> do 
                             r2 <- popResM
                             r1 <- popResM
                             case (r2,r1) of
                               (ResPhrase (IP (IntVal n2)),ResPhrase (IP (IntVal n1))) ->
                                 case op of   
                                   Eq -> pushResM (ResPhrase (BP (BoolVal (n1 == n2))))
                                   Gt -> pushResM (ResPhrase (BP (BoolVal (n1 > n2))))
                                   Lt -> pushResM (ResPhrase (BP (BoolVal (n1 < n2))))
                               otherwise -> ExceptT (return (Left "BOp: Malformed stack")) 

            {- Commands -}
            (CtlPhrase (CP exp)) -> case exp of
              Skip -> return ()             
              --Assign Ident IntExp
              --Seq Command Command
              --If BoolExp Command Command
              --While BoolExp Command

            --CtlWhile -> undefined

            {- Major fuck up .. Actually not possible if all Constructers are covered -}
            otherwise -> ExceptT (return (Left "Unknown element on control stack"))


{- Check if the state is terminal (for the moment == empty control stack) -}
isTerminalStateM :: (ExceptT String) (State SMC) Bool
isTerminalStateM = ExceptT (do
                              (cs,rs,s) <- get
                              return (Right (null cs)))
   
{- 
Pop the control stack and modify the state accordingly
until a terminal state is achieved 
-}
executeM :: (ExceptT String) (State SMC) ()
executeM = do
             b <- isTerminalStateM
             if b 
             then return ()
             else do stepM
                     executeM

{- Convenience function for running programs -}
runM :: SMC -> SMC
runM s = runIdentity (((execStateT . runExceptT) executeM) s)

{- Operations on the stacks -}

popCtlM :: (ExceptT String) (State SMC) Control
popCtlM = ExceptT (do (cs,rs,s) <- get
                      if null cs 
                      then return (Left "Attempt to pop empty stack")
                      else do put (tail cs,rs,s)  
                              return (Right (head cs)))

pushCtlM :: Control -> (ExceptT String) (State SMC) ()
pushCtlM c = ExceptT (do (cs,rs,s) <- get
                         put (c:cs,rs,s)
                         return (Right ()))

popResM :: (ExceptT String) (State SMC) Result
popResM = ExceptT (do (cs,rs,s) <- get
                      if null rs 
                      then return (Left "Attempt to pop empty result stack") 
                      else do put (cs,tail rs,s)
                              return (Right (head rs)))

pushResM :: Result -> (ExceptT String) (State SMC) ()
pushResM r = ExceptT (do (cs,rs,s) <- get 
                         put (cs,r:rs,s)
                         return (Right ()))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
