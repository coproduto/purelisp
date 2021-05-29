module Language.PureLisp.Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.Prim

data InputMetadata = InputMetadata
  { metadataLine :: Int
  , metadataColumn :: Int
  } deriving Show

data LispInput
  = Atom InputMetadata String
  | Pair LispInput LispInput
  deriving Show

type Parser = Parsec String ()

getInputMetadata :: Parser InputMetadata
getInputMetadata = do
  position <- statePos `liftM` getParserState
  return $ InputMetadata
    { metadataLine = sourceLine position
    , metadataColumn = sourceColumn position
    }

parseAtom :: Parser LispInput
parseAtom = do
  meta <- getInputMetadata
  first <- letter
  rest <- many (letter <|> digit)
  return $ Atom meta (first:rest)

parseHsList :: Parser [LispInput]
parseHsList = parseLispInput `sepBy` spaces

parseConsListItems :: Parser LispInput
parseConsListItems = do
  pos <- getPosition
  list <- parseHsList
  meta <- getInputMetadata
  return $ foldr Pair (Atom meta "nil") list

parseConsList :: Parser LispInput
parseConsList = do
  char '('
  list <- parseConsListItems
  char ')'
  return list

parseLispInput :: Parser LispInput
parseLispInput = parseAtom <|> parseConsList
