{-
 Functional parsing
 Hutton p 177
-}
module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) s = p s  

item :: Parser Char
item = P (\s -> case s of 
                  "" -> []
                  (c:cs) -> [(c,cs)])

instance Functor Parser where
  -- fmap (a -> b) -> (Parser a -> Parser b)  
  fmap g p = P (\inp -> case parse p inp of
                          [] -> []  
                          [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- <*> Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of 
                           [] -> []
                           [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  --return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p g = P (\inp -> case parse p inp of
                           [] -> []
                           [(v,out)] -> parse (g v) out)  

instance Alternative Parser where 
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) px py = P (\inp -> case parse px inp of 
                             [] -> parse py inp
                             [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat f = do
          c <- item
          if (f c) then return c else empty

digit :: Parser Char
digit = sat isDigit
 
lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String 
string [] = return []
string (x:xs) = do
                  char x 
                  string xs 
                  return (x:xs)
--some, many from Applicative
ident :: Parser String
ident = do 
          x <- lower
          xs <- many alphanum
          return (x:xs)
 
nat :: Parser Int
nat = do 
        xs <- some digit 
        return (read xs)

space :: Parser ()
space = do
          many (sat isSpace)
          return ()

int :: Parser Int
int = do
        char '-'
        n <- nat
        return (-n)
       <|> nat 

token :: Parser a -> Parser a 
token p = do 
            space 
            v <- p
            space
            return v

identifier :: Parser String 
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int 

symbol :: String -> Parser String 
symbol xs = token (string xs)

 
{-
examples -- Applicative 
-}
pair :: Parser (Char,Char)
pair = pure (,) <*> item <*> item

andToUpper :: Parser (Char,Char)
andToUpper = pure (\x -> (x,toUpper x)) <*> item


{-
examples Monadic
-}

mpair :: Parser (Char,Char)
mpair = do
          x <- item
          y <- item
          return (x,y)

mAndToUpper :: Parser (Char,Char)
mAndToUpper = do
                x <- item
                return (x, toUpper x)













