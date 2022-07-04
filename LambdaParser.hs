module LambdaParser where

import Control.Applicative
import Data.Char
import Lambda
import Parser

parseExp :: String -> Maybe Exp
parseExp s =
  case runParser expP s of
    Just ("", e) -> Just e
    _ -> Nothing

expP :: Parser Exp
expP = do
  es <- space0 *> some ((absP <|> atomP) <* space0)
  case es of
    [e] -> return e
    f : e : es -> return $ foldl App (App f e) es
    [] -> error "unreachable"

atomP :: Parser Exp
atomP = refP <|> parens expP

refP :: Parser Exp
refP = Ref <$> varP

absP :: Parser Exp
absP = Abs <$> lamP

appP :: Parser Exp
appP = App <$> expP <* space1 *> expP

varP :: Parser Var
varP = some $ satisfy isVar

lamP :: Parser Lam
lamP = (:=>) <$> (char '\\' *> varP) <* pad (char '.') <*> expP

isVar :: Char -> Bool
isVar c = isAlpha c || c == '\'' || c == '_' || c == '-'

space :: Parser Char
space = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

space0 :: Parser String
space0 = many space

space1 :: Parser String
space1 = some space

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

pad :: Parser a -> Parser a
pad = between space0 space0
