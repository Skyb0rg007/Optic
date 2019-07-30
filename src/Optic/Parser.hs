{-# LANGUAGE OverloadedStrings #-}

module Optic.Parser
    ( parseExpr
    , parseDecl
    ) where

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.Orphans         ()

import           Optic.AST

type Parser = Parsec Void Text

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
reserved = ["if", "then", "else", "let", "in", "true", "false"]

---

-- TODO
parseDecl :: Parser [OpticExpr]
parseDecl = some parseExpr

parseExpr :: Parser OpticExpr
parseExpr = foldl1 App <$> some parseTerm

parseTerm :: Parser OpticExpr
parseTerm =
        try parseVar
    <|> parseParen
    <|> Literal <$> parseLit
    <|> parseLet
    <|> parseLam
    where
        parseVar = label "variable" $ optVar <$> identifier
        parseParen = between (symbol "(") (symbol ")") parseExpr
        parseLet = label "let" $ do
            _ <- symbol "let"
            x <- identifier
            _ <- symbol "="
            e1 <- parseExpr
            _ <- symbol "in"
            optLet x e1 <$> parseExpr
        parseLam = label "lambda" $ do
            _ <- symbol "\\"
            x <- identifier
            _ <- symbol "->"
            optLambda x <$> parseExpr

parseLit :: Parser OpticLit
parseLit =
        parseInt
    <|> parseBool
    <|> parseText
    <|> parseChar
    where
        parseInt = label "int" $ LitInt <$> intLiteral
        parseBool = label "bool" $
                LitBool True  <$ symbol "true"
            <|> LitBool False <$ symbol "false"
        parseText = label "text" $ LitText <$> stringLiteral
        parseChar = label "char" $ LitChar <$> charLiteral

