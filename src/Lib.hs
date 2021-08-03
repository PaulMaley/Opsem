module Lib where

import qualified Data.Map as Map
import Control.Monad.Trans.State.Strict


-- REDESIGN THE FUCKING TYPES !!! THEY ARE SHIT
-- Possible to do while remaining faithful to the course ??

{- Data types -}

-- Phrase (strange name)
data Phrase = CP Command
            | IP IntExp
            | BP BoolExp
            deriving (Show)

data Command = Skip
             | Assign Ident IntExp
             | Seq Command Command
             | If BoolExp Command Command
             | While BoolExp Command
             deriving (Show)

data IntExp = IntVal Int
            | DeRef Ident
            | IntOpExp IntOp IntExp IntExp
            deriving (Show)

data BoolExp = BoolVal Bool
             | BoolOpExp BoolOp IntExp IntExp
             deriving (Show)

type Ident = String 

-- To complete ....
data IntOp = Add
           | Mult
           deriving (Show)

data BoolOp = EQ
            | GT
            deriving (Show)


data Control = CtlPhrase Phrase
             | CtlIOp IntOp
             | CtlBOp BoolOp
             | CtlWhile
             deriving (Show)

data Result = ResPhrase Phrase
            | ResIdent Ident 
            deriving (Show)

type Store = Map.Map Ident Int
emptyStore :: Store
emptyStore = Map.empty

extendStore :: Ident -> Int -> Store -> Store
extendStore id val s = Map.insert id val s

findInStore :: Ident -> Store -> Maybe Int
findInStore = Map.lookup  

findInStoreM :: Ident -> State SMC (Maybe Int)
findInStoreM id = do 
                    (_,_,s) <- get
                    return (findInStore id s) 
 
--instance Show Store where
--  show _ = "[]"

{- 
Machine state: A triple:
 ControlStack :: [Control]
 ResultStack ::  [Result]
 Store :: Ident -> IntVal

Put these into a State Monad 
 ([Control], [Result], Store) -> ((), ([Control], [Result], Store))
-}

type SMC = ([Control],[Result],Store)  

type SMCm = StateT SMC (Either String) ()

-- load a program
--     loadm :: Phrase -> Store -> State SMC () 
--     loadm p s = State (\_ -> ((),([CtlPhrase p], [], s)))
--loadm :: Phrase -> Store -> StateT SMC (Either String) () 
--loadm p s =  state (\(_,_,_) -> ((), ([CtlPhrase p], [], s))) 

load :: Phrase -> Store -> SMC
load p s = ([CtlPhrase p], [], s)


--execute :: StateT SMC (Either String) ()
--execute = undefined

-- Step the machine
-- Pattern match on the control stack
step :: SMC -> SMC
--step ([],_,s) = ([],[],s) 
step (cs, rs, s) = case cs of
  [] -> ([],[],s)
  ((CtlPhrase (IP (IntVal n))):cs') -> (cs', (ResPhrase (IP (IntVal n))):rs, s)
  ((CtlPhrase (IP (DeRef id))):cs') -> case findInStore id s of 
                                         Nothing -> error ("Undefined identifier " ++id)
                                         (Just n) -> (cs', (ResPhrase (IP (IntVal n))):rs, s)
  
  ((CtlPhrase (IP (IntOpExp iop ie1 ie2))):cs') -> ((CtlPhrase (IP (ie1))):
                                                    (CtlPhrase (IP (ie2))):
                                                    (CtlIOp  iop):cs', rs, s)
  ((CtlIOp op):cs') -> let ((ResPhrase (IP (IntVal n2))):(ResPhrase (IP (IntVal n1))):rs') = rs 
                       in case op of
                                 Add -> (cs', (ResPhrase (IP (IntVal (n1 + n2)))):rs', s)
  

--stepm :: () -> StateT SMC (Either String) ()
stepm :: State SMC ()
stepm = do
          c <- popCtl
          case c of 
            {- Int expressions -}
            (CtlPhrase (IP exp)) -> case exp of
               (IntVal n) -> pushRes (ResPhrase (IP (IntVal n)))
               (DeRef id) -> do 
                               val <- findInStoreM id 
                               case val of
                                  Nothing -> error ("Undefined identifier " ++id)
                                  (Just n) -> pushRes (ResPhrase (IP (IntVal n)))
               (IntOpExp iop ie1 ie2) -> do pushCtl (CtlIOp  iop)
                                            pushCtl (CtlPhrase (IP (ie2)))
                                            pushCtl (CtlPhrase (IP (ie1)))
            (CtlIOp op) -> do
                             r2 <- popRes
                             r1 <- popRes
                             case (r2,r1) of
                               (ResPhrase (IP (IntVal n2)),ResPhrase (IP (IntVal n1))) ->
                                 case op of   
                                   Add -> pushRes (ResPhrase (IP (IntVal (n1 + n2))))
                                   Mult -> undefined
                               otherwise -> error "Malformed stack"       

            {- Bool expressions -}
            (CtlPhrase (BP exp)) -> case exp of 
              (BoolVal b) -> undefined
              (BoolOpExp bop ie1 iee) -> undefined
            (CtlBOp op) -> undefined

            {- Operators etc. -}
            CtlWhile -> undefined

            {- Major fuck up-}
            otherwise -> error "Unknown element on control stack" --put ([],[],emptyStore)

{- Check to see if we are in a terminal state -}
{- NEEDS COMPLETING -}
isTerminal :: State SMC Bool
isTerminal = do
               (cs,rs,s) <- get
               return (null cs)

{- 
Pop the control stack and modify the state accordingly
until a terminal state is achieved 
-}

execute :: State SMC ()
execute = do
            finished <- isTerminal
            if finished 
            then return ()
            else do stepm
                    execute

{- Operations on the stacks -}
popCtl :: State SMC Control
popCtl = do 
            (cs,rs,s) <- get
            if null cs 
            then (error "Attempt to pop empty control stack") 
            else do put (tail cs,rs,s)
                    return (head cs)
{-
            else let (c:cs') = cs 
                 in do 
                      put (cs',rs,s)
                      return c
-}

pushCtl :: Control -> State SMC ()
pushCtl c = do                 
              (cs,rs,s) <- get
              put (c:cs,rs,s)

popRes :: State SMC Result
popRes = do 
            (cs,rs,s) <- get
            if null rs 
            then (error "Attempt to pop empty result stack") 
            else do put (cs,tail rs,s)
                    return (head rs)
{-
            else let (r:rs') = rs 
                 in do 
                      put (cs,rs',s)
                      return r
-}
pushRes :: Result -> State SMC ()
pushRes r = do                 
              (cs,rs,s) <- get
              put (cs,r:rs,s)






           



someFunc :: IO ()
someFunc = putStrLn "someFunc"
