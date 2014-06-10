{-# Language FlexibleContexts, RankNTypes #-}
{-# Language ImpredicativeTypes, NoMonomorphismRestriction #-}
module Parser where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

import Control.Arrow
import Control.Applicative

import Data.ListLike (ListLike)

import Lambda

extraLamSyms :: [String]
extraLamSyms  = ["λ"]

extraDotSyms :: [String]
extraDotSyms = ["→"]

extraAppSyms :: [String]
extraAppSyms = []

parseSymAndSyms :: ( IsLocationUpdatedBy loc Char
                   , ListLike state Char
                   ) =>
                   String -> 
                   P (Str Char state loc) String -> 
                   P (Str Char state loc) String
parseSymAndSyms p ps = (lexeme (pSymbol p) <* pSpaces) <|> ps

parseTermSym :: ( IsLocationUpdatedBy loc Char
                , ListLike state Char
                ) =>
                P (Str Char state loc) String ->
                [String] ->
                P (Str Char state loc) String
parseTermSym = foldr parseSymAndSyms

parseLamSym :: Parser String
parseLamSym = parseTermSym (lexeme $ pSymbol "\\") extraLamSyms

parseDotSym :: Parser String
parseDotSym = parseTermSym (lexeme $ pSymbol ".") extraDotSyms

parseAppSym :: Parser String
parseAppSym = parseTermSym (lexeme $ pSymbol "@") extraDotSyms

parseVar :: Parser Var
parseVar = lexeme $ many pLetter

parseId :: [Var] -> Parser Term
parseId vars = Id <$> parseTermSym pFail vars

parseApp :: [Var] -> Parser Term
parseApp vars = (App <$ parseAppSym) `pChainl` (parseTerm' vars)

parseAbs :: [Var] -> Parser Term
parseAbs vars = join $ uncurry (<$>) 
                    <$> 
                (Abs &&& parseTerm . (:vars)) <$ parseLamSym <*> parseVar <* parseDotSym

parseTerm' :: [Var] -> Parser Term
parseTerm' vars =  parseId vars
               <|> parseAbs vars
               -- <|> pParens (parseTerm vars)

parseTerm :: [Var] -> Parser Term
parseTerm vars = parseApp vars
