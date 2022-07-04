module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \i -> do
    (i', x) <- p i
    return (i', f x)

instance Applicative Parser where
  pure x = Parser $ \i -> Just (i, x)
  (Parser p) <*> (Parser q) = Parser $ \i -> do
    (i', f) <- p i
    (i'', x) <- q i'
    return (i'', f x)

instance Monad Parser where
  p >>= f = Parser $ \i -> case runParser p i of
    Nothing -> Nothing
    Just (i', x) -> runParser (f x) i'

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p) <|> (Parser q) = Parser $ \i -> p i <|> q i

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \i -> case i of
  c : cs | pred c -> Just (cs, c)
  _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string "" = pure ""
string (c : cs) = (:) <$> char c <*> string cs

between :: Parser b -> Parser c -> Parser a -> Parser a
between p q r = p *> r <* q
