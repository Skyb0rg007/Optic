{-# LANGUAGE OverloadedStrings #-}

module Optic.Parser where

import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Optic.AST

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

constructor :: Parser Text
constructor = lexeme $
    fmap T.pack $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')

reserved :: [Text]
reserved = ["if", "then", "else", "let", "in", "effect", "case", "end"]

---

-- An expression is:
--  Function application if terms are in a list
--  Sequencing if separated with ';' - syntax sugar over let _ = e1 in e2
parseExpr :: Parser OpticExpr
parseExpr =
        wrapSequence <$> parseApp `sepEndBy1` symbol ";"
    where
        wrapSequence []     = error "This cannot happen"
        wrapSequence [e]    = e
        wrapSequence (e:es) = Let "_" e (wrapSequence es)
        parseApp = foldl1 App <$> some parseTerm

-- A pattern is:
--  A variable
--  A constructor followed by constructors/patterns
parsePattern :: Parser OpticPattern
parsePattern =
        parseCon
    <|> parsePatternTerm
    where
        parseCon = label "constructor" $ PatCon <$> constructor <*> some parsePatternTerm

-- A pattern term is:
--  A variable
--  A unary constructor
--  A patrenthesized pattern
parsePatternTerm :: Parser OpticPattern
parsePatternTerm =
        parseVar
    <|> parseCon
    <|> PatLit <$> parseLit
    <|> parseParen
    where
        parseVar = label "variable" $ PatVar <$> identifier
        parseCon = label "unary constructor" $ PatCon <$> constructor <*> pure []
        parseParen = between (symbol "(") (symbol ")") parsePattern

-- A term is:
--  A variable
--  A parenthesized expression
--  A literal
--  A let expression
--  A lambda expression
--  A case expression
parseTerm :: Parser OpticExpr
parseTerm =
        try parseVar
    <|> try parseParen
    <|> Literal <$> parseLit
    <|> parseLet
    <|> parseLam
    <|> parseCase
    where
        parseVar = label "variable" $ Var <$> identifier
        parseParen = between (symbol "(") (symbol ")") parseExpr
        parseLet = label "let" $ do
            _ <- symbol "let"
            x <- identifier
            _ <- symbol "="
            e1 <- parseExpr
            _ <- symbol "in"
            Let x e1 <$> parseExpr
        parseLam = label "lambda" $ do
            _ <- symbol "\\"
            x <- identifier
            _ <- symbol "->"
            Lambda x <$> parseExpr
        parseCase = label "case" $ do
            _ <- symbol "case"
            e <- parseExpr
            c:cs <- some (try parseEffExpr <|> parseCaseExpr)
            _ <- symbol "end"
            pure $ Case e (c:|cs)
        parseCaseExpr = label "case expr" $ do
            _ <- symbol "|"
            p <- parsePattern
            _ <- symbol "->"
            CaseMatch p <$> parseExpr
        parseEffExpr = label "eff expr" $ do
            _ <- symbol "|"
            _ <- symbol "effect"
            p <- parsePatternTerm
            k <- identifier
            _ <- symbol "->"
            CaseEff p k <$> parseExpr

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
        parseText = label "text" $ LitText <$> stringLiteral
        parseChar = label "char" $ LitChar <$> charLiteral
        parseUnit = label "unit" $ LitUnit <$ symbol "()"
