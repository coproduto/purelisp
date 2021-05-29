module Language.PureLisp.Parser where

import Control.Applicative ((*>))
import Control.Monad (liftM)

import Text.Parsec

data InputMetadata = InputMetadata
  { metadataLine :: Int
  , metadataColumn :: Int
  } deriving Show

data LispInput
  = Atom InputMetadata String
  | Pair LispInput LispInput
  | ListMeta InputMetadata LispInput
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
parseHsList = spaces *> parseLispInput `sepEndBy` spaces

parseConsListItems :: Parser LispInput
parseConsListItems = do
  list <- parseHsList
  meta <- getInputMetadata
  return $ foldr Pair (Atom meta "nil") list

parseConsList :: Parser LispInput
parseConsList = do
  meta <- getInputMetadata
  char '('
  list <- parseConsListItems
  char ')'
  return $ ListMeta meta list

parseLispInput :: Parser LispInput
parseLispInput = parseAtom <|> parseConsList
