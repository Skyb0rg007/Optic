{-# LANGUAGE OverloadedStrings #-}

module Optic.Core.Parse
    ( parseExpr
    ) where

import           Bound
import           Data.List                  (elemIndex)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Optic.Core.Expr

-- No custom errors
type Parser = Parsec Void Text

---

spaceConsume :: Parser ()
spaceConsume = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#[[" "]]")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsume

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsume

intLiteral :: Parser Int
intLiteral = label "int" $ lexeme L.decimal

charLiteral :: Parser Char
charLiteral = label "char" $ lexeme $ between "'" "'" L.charLiteral

stringLiteral :: Parser Text
stringLiteral = label "string" $ lexeme $
    fmap T.pack $ char '"' >> manyTill L.charLiteral (char '"')

identifier :: Parser Text
identifier = lexeme $ do
    ident <- fmap T.pack $ (:) <$> (lowerChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    if ident `notElem` reserved
       then pure ident
       else fail $ T.unpack ident ++ " is a reserved word"

reserved :: [Text]
reserved = ["if", "then", "else", "let", "in", "effect", "case", "end"]

constructor :: Parser Int
constructor = between (symbol "<") (symbol ">") intLiteral

---

parseExpr :: Parser (OpticExpr Text)
parseExpr = foldl1 App <$> some parseTerm

parseTerm :: Parser (OpticExpr Text)
parseTerm =
        try parseVar
    <|> try parseParen
    <|> try parseLet
    <|> try parseLam
    <|> try parseCase
    <|> Literal <$> parseLit
    where
        parseVar = label "variable" $ Var <$> identifier
        parseParen = between (symbol "(") (symbol ")") parseExpr
        parseLet = label "let" $ do
            _ <- symbol "let"
            lines <- parseLetLine `sepEndBy1` symbol ";"
            _ <- symbol "in"
            let_ lines <$> parseExpr
        parseLetLine = label "let line" $ do
            x <- identifier
            _ <- symbol "="
            e <- parseExpr
            pure (x, e)
        parseLam = label "lambda" $ do
            _ <- symbol "\\"
            x <- identifier
            _ <- symbol "->"
            lam x <$> parseExpr
        parseCase = label "case" $ do
            _ <- symbol "case"
            e <- parseExpr
            cases <- some parseCaseExpr
            _ <- symbol "end"
            pure $ Case e cases
        parseCaseExpr = try parseCaseMatchExpr <|> parseCaseVarExpr
        parseCaseMatchExpr :: Parser (OpticPat, Scope Int OpticExpr Text)
        parseCaseMatchExpr = label "case expr" $ do
            _ <- symbol "|"
            c <- constructor
            args <- many identifier
            _ <- symbol "->"
            e <- parseExpr
            let abstr :: Text -> Maybe Int
                abstr "_" = Nothing
                abstr v   = elemIndex v args
            pure (PatConstructor c (length args), abstract abstr e)
        parseCaseVarExpr :: Parser (OpticPat, Scope Int OpticExpr Text)
        parseCaseVarExpr = label "case var" $ do
            _ <- symbol "|"
            ident <- identifier
            _ <- symbol "->"
            e <- parseExpr
            let abstr :: Text -> Maybe Int
                abstr "_" = Nothing
                abstr _   = Just 0
            pure (PatVariable, abstract abstr e)



-- A literal is:
--  An integer
--  A boolean
--  A string
--  A character
--  A unit value
parseLit :: Parser OpticLit
parseLit =
        parseInt
    <|> parseBool
    <|> parseText
    <|> parseChar
    <|> parseUnit
    where
        parseInt = label "int" $ LitInt <$> intLiteral
        parseBool = label "bool" $
                LitBool True  <$ symbol "True"
            <|> LitBool False <$ symbol "False"
        parseText = label "text" $ LitString <$> stringLiteral
        parseChar = label "char" $ LitChar <$> charLiteral
        parseUnit = label "unit" $ LitUnit <$ symbol "()"
