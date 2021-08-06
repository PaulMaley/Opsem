{-
A parser for the SMC machine language LC
Using Hutton's parsing code copied from the Language project
-}
module ParseLC where

import DataTypes
import Parser
import Control.Applicative

{-
The concrete language - try to make non-ambiguous !! 

<Phrase>  -> <Command>
           | <IntExp>
           | <BoolExp>
<Integer> -> As defined in the parser program
<Ident>   -> String
<IntExp>  -> <Integer> 
           | <Iop>(<IntExp>,<IntExp>)
           | !<Ident>
<Iop>     -> + | - | *
<BoolExp> -> True | False | <Bop>(<IntExp>,<IntExp>)  
<Bop>     -> = | > | < 
<Command> -> Skip
           | :=(<Ident>,>IntExp>)
           | If(<BoolExp>,<Command>,<Command>)
           | While(<BoolExp>,<Command>)
           | Seq(<Command>,<Command>) 

Look at changing Seq to something sweeter: 
 {C1;C2;C3;...} for example 

The abstract language is as defined in DataTypes.hs
Phrase ::= Command | IntExp | BoolExp

Command ::= Skip 
          | Ident := IntExp 
          | Command ; Command
          | If BoolExp Then Command Else Command
          | While BoolExp Do Command

IntExp ::= Integer | !Ident | IntExp Iop IntExp

Iop ::= + | - | * 

BoolExp ::= True | False | IntExp Bop IntExp

Bop ::= > | < | = 
-}

lcphrase :: Parser Phrase
lcphrase = do ie <- lcintexp
              return (IP ie) 
             <|> do cmd <- lccommand
                    return (CP cmd)

lcintexp :: Parser IntExp
lcintexp = do n <- integer 
              return (IntVal n)
             <|> lciopexp
             <|> lcderef

lciopexp :: Parser IntExp
lciopexp = do iop <- lciop
              symbol "("
              ie1 <- lcintexp
              symbol ","
              ie2 <- lcintexp
              symbol ")"
              return (IntOpExp iop ie1 ie2)

lciop :: Parser IntOp
lciop = do symbol "+"
           return Add
          <|> do symbol "-"
                 return Sub
          <|> do symbol "*"
                 return Mult

lcderef :: Parser IntExp
lcderef = do symbol "!"
             id <- identifier
             return (DeRef id)

lccommand :: Parser Command
lccommand = do lcskip
              <|> lcassign
              <|> lcwhile
              <|> lcif
              <|> lcseq

lcskip :: Parser Command 
lcskip = do symbol "Skip"
            return Skip

lcassign :: Parser Command
lcassign = do symbol ":=("
              id <- identifier
              symbol ","
              ie <- lcintexp
              symbol ")"
              return (Assign id ie)

lcwhile :: Parser Command
lcwhile = do symbol "While("
             bexp <- lcboolexp
             symbol ","
             cmd <- lccommand
             symbol ")"
             return (While bexp cmd)

lcif :: Parser Command
lcif = do symbol "If("
          bexp <- lcboolexp
          symbol ","
          cmdt <- lccommand
          symbol ","
          cmdf <- lccommand
          symbol ")"
          return (If bexp cmdt cmdf)  

lcseq :: Parser Command
lcseq = do symbol "Seq("
           c1 <- lccommand
           symbol ","
           c2 <- lccommand
           symbol ")"
           return (Seq c1 c2)

lcboolexp :: Parser BoolExp
lcboolexp = do symbol "True"
               return (BoolVal True)
             <|> do symbol "False"
                    return (BoolVal False)
             <|> lcbopexp

lcbopexp :: Parser BoolExp
lcbopexp = do bop <- lcbop
              symbol "("
              ie1 <- lcintexp
              symbol ","
              ie2 <- lcintexp
              symbol ")"
              return (BoolOpExp bop ie1 ie2)

lcbop :: Parser BoolOp
lcbop = do symbol "="
           return Eq
          <|> do symbol ">"
                 return Gt
          <|> do symbol "<"
                 return Lt


