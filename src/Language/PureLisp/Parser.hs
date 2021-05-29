module Language.PureLisp.Parser where

import Text.Parsec

data LispInput
  = Atom String
  | Pair LispInput LispInput
  deriving Show

type Parser = Parsec String ()

parseAtom :: Parser LispInput
parseAtom = do
  first <- letter
  rest <- many (letter <|> digit)
  return $ Atom (first:rest)

parseHsList :: Parser [LispInput]
parseHsList = parseLispInput `sepBy` spaces

parseConsListItems :: Parser LispInput
parseConsListItems = foldr Pair (Atom "nil") <$> parseHsList

parseConsList :: Parser LispInput
parseConsList = do
  char '('
  list <- parseConsListItems
  char ')'
  return list

parseLispInput :: Parser LispInput
parseLispInput = parseAtom <|> parseConsList
