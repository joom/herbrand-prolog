module Prolog.Parser where

import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H
import qualified Herbrand.Model as M
import qualified Data.Char as C

import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

parseProgram :: Parser H.Program
parseProgram = do
    first <- parseHornClause
    optional spaces
    next <- parseProgram <|> (return [])
    return (first:next)

parseHornClause :: Parser H.HornClause
parseHornClause = do
    hd <- parseFormula
    (char '.' >> return (H.HornClause hd [])) <|> do
      char ':' >> char '-'
      tl <- parseFormulae
      char '.'
      return $ H.HornClause hd tl

parseFormula :: Parser H.Formula
parseFormula = do
    optional spaces
    relName <- many (letter <|> digit)
    optional spaces
    char '('
    terms <- parseTerms
    char ')'
    optional spaces
    return $ H.Formula (H.R relName (length terms)) terms

parseFormulae :: Parser [H.Formula]
parseFormulae = do
    optional spaces
    first <- parseFormula
    optional spaces
    next <- (char ',' >> parseFormulae) <|> (return [])
    optional spaces
    return (first:next)

-- should handle functions
parseTerm :: Parser H.Term
parseTerm = do
  name <- many letter
  return (if   C.isUpper (head name)
          then H.BrV (H.V name)
          else H.BrC (H.C name))

parseTerms :: Parser [H.Term]
parseTerms = do
    optional spaces
    first <- parseTerm
    optional spaces
    next <- (char ',' >> parseTerms) <|> (return [])
    optional spaces
    return (first:next)

readHornClause :: String -> H.HornClause
readHornClause input = case parse parseHornClause "herbrand-prolog" input of
    Left err -> error $ show err
    Right val -> val

readProgram :: String -> H.Program
readProgram input = case parse parseProgram "herbrand-prolog" input of
    Left err -> error $ show err
    Right val -> val

readProgramFromFile :: String -> IO H.Program
readProgramFromFile filename = do
    result <- parseFromFile parseProgram filename
    return (case result of
              Left err -> error $ show err
              Right val -> val)
