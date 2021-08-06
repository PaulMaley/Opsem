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
import DataTypes

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

extendStoreM :: Ident -> Int -> (ExceptT String) (State SMC) ()
extendStoreM id n = ExceptT (do (cs,rs,s) <- get
                                let s' = extendStore id n s
                                put (cs,rs,s')
                                return (Right ()))

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
                                            pushCtlM (CtlPhrase (IP ie2))
                                            pushCtlM (CtlPhrase (IP ie1))
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

            {- Command expressions -}
            (CtlPhrase (CP exp)) -> case exp of
              Skip -> return ()             
              (Assign id iexp) -> do
                                    pushResM (ResIdent id)
                                    pushCtlM (CtlAssign)
                                    pushCtlM (CtlPhrase (IP iexp))
              (Seq cmd1 cmd2) -> do pushCtlM (CtlPhrase (CP cmd2))
                                    pushCtlM (CtlPhrase (CP cmd1)) 
              (If bexp cmdt cmdf) -> do pushCtlM (CtlIf)
                                        pushCtlM (CtlPhrase (BP bexp))
                                        pushResM (ResPhrase (CP cmdf))
                                        pushResM (ResPhrase (CP cmdt))
              (While bexp cmd) -> do pushResM (ResPhrase (CP cmd))
                                     pushResM (ResPhrase (BP bexp))
                                     pushCtlM CtlWhile
                                     pushCtlM (CtlPhrase (BP bexp))            

            {- Naked commands -}
            CtlAssign -> do val <- popResM
                            id <- popResM
                            case (id,val) of 
                              (ResIdent s, ResPhrase (IP (IntVal n))) -> extendStoreM s n
                              otherwise -> ExceptT (return (Left "Assign: Malformed stack"))
{-
            CtlIf -> do b <- popResM
                        ct <- popResM
                        cf <- popResM      
                        case (b,ct,cf) of
                          ((ResPhrase (BP (BoolVal True))),
                           (ResPhrase (CP ct)),
                           (ResPhrase (CP cf))) -> pushCtlM (CtlPhrase (CP ct)) 
                          ((ResPhrase (BP (BoolVal False))),
                           (ResPhrase (CP ct)),
                           (ResPhrase (CP cf))) -> pushCtlM (CtlPhrase (CP cf)) 
                          otherwise -> ExceptT (return (Left "CtlIf: Malformed stack"))
-}            
            CtlIf -> do b <- popResMBoolVal
                        ct <- popResMCmd
                        cf <- popResMCmd
                        case b of 
                          True -> pushCtlM (CtlPhrase (CP ct)) 
                          False -> pushCtlM (CtlPhrase (CP cf))    

            -- Try somethin a bit more intelligent .... a new function popResMBoolVal !!
            -- TODO Make more functions like this and rewrite the above !!!!!! 
            CtlWhile -> do b <- popResMBoolVal
                           case b of 
                             True -> do bexp <- popResMBoolExp
                                        cmd <- popResMCmd 
                                        pushCtlM (CtlPhrase (CP (While bexp cmd)))
                                        pushCtlM (CtlPhrase (CP cmd))
                             False -> do popResMBoolExp  -- While loop ends, throw away <bexp> and <cmd>
                                         popResMCmd      -- Check types rather than just throw away
                                         return ()

            {- Major fuck up .. Actually not possible if all Constructers are covered -}
            --otherwise -> ExceptT (return (Left "Unknown element on control stack"))


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

{- 
functions that require specific types from the stack or 
throw an error (TODO include the stacks in the error message ??)
-}
popResMBoolVal :: (ExceptT String) (State SMC) Bool
popResMBoolVal = do val <- popResM
                    case val of
                      (ResPhrase (BP (BoolVal b))) -> ExceptT (return (Right b))
                      otherwise -> ExceptT (return (Left "popResMBoolVal: Attempt to pop non boolean"))

popResMBoolExp :: (ExceptT String) (State SMC) BoolExp
popResMBoolExp = do val <- popResM
                    case val of
                      (ResPhrase (BP bexp)) -> ExceptT (return (Right bexp))
                      otherwise -> ExceptT (return (Left "popResMBoolExp: Attempt to pop non boolean"))

popResMCmd :: (ExceptT String) (State SMC) Command
popResMCmd = do val <- popResM
                case val of
                  (ResPhrase (CP cmd)) -> ExceptT (return (Right cmd))
                  otherwise -> ExceptT (return (Left "popResMCmd: Expected Command, found other"))

{- Raw operations on the stacks -}

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
