{-# LANGUAGE OverloadedStrings #-}
module Language.PureLisp.Parser where

import Control.Applicative ((*>))
import Control.Monad (liftM)

import qualified Data.Text as Text
import Data.Text (Text)

import Text.Parsec

type Parser = Parsec Text ()

data InputMetadata = InputMetadata
  { metadataLine :: Int
  , metadataColumn :: Int
  } deriving Show

data LispInput
  = Atom InputMetadata Text
  | Pair LispInput LispInput
  | ListMeta InputMetadata LispInput
  deriving Show

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
  return $ Atom meta (Text.pack (first:rest))

parseHsList :: Parser [LispInput]
parseHsList = spaces *> parseLispInput `sepEndBy` spaces

parseConsListItems :: Parser LispInput
parseConsListItems = do
  list <- parseHsList
  meta <- getInputMetadata
  return $ foldr Pair (Atom meta "()") list

parseConsList :: Parser LispInput
parseConsList = do
  meta <- getInputMetadata
  char '('
  list <- parseConsListItems
  char ')'
  return $ ListMeta meta list

parseLispInput :: Parser LispInput
parseLispInput = parseAtom <|> parseConsList
