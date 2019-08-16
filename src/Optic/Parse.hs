{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Optic.Parse
    ( parseLit
    , parseTy
    , parseExp
    ) where

-- import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Data.Int                   (Int64)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Data.Word                  (Word64)
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

intLiteral :: Parser Int64
intLiteral = label "int" $ lexeme L.decimal

wordLiteral :: Parser Word64
wordLiteral = label "word" $ lexeme (L.decimal <* char 'u')

floatLiteral :: Parser Double
floatLiteral = label "float" $ lexeme L.float

charLiteral :: Parser Char
charLiteral = label "char" $ lexeme $ between "'" "'" L.charLiteral

stringLiteral :: Parser String
stringLiteral = label "string" $ lexeme $
    char '"' >> manyTill L.charLiteral (char '"')

identifier :: Parser Text
identifier = lexeme $ do
    ident <- fmap T.pack $ (:) <$> (lowerChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    if ident `notElem` reserved
       then pure ident
       else fail $ T.unpack ident ++ " is a reserved word"

reserved :: [Text]
reserved = ["if", "then", "else", "let", "in", "effect", "case", "end"]

---

parseExp :: Parser Exp
parseExp = foldl1 ExpApp <$> some parseExpAtom

parseExpAtom :: Parser Exp
parseExpAtom = label "expression" $
        try (ExpLit <$> parseLit)
    <|> try parseLet
    <|> try (ExpLamVar <$> identifier)
    <|> try (ExpLetVar <$> (char '^' >> identifier))
    <|> try parseLam
    <|> try parseRecord
    <|> try parseTyped
    <|> try parseTyped
    <|> between (symbol "(") (symbol ")") parseExp
    where
        parseLet = do
            _ <- symbol "let"
            bs <- parseLetExp `sepBy1` symbol ";"
            _ <- symbol "in"
            e <- parseExp
            _ <- symbol "end"
            pure $ ExpLet bs e
        parseLetExp = do
            x <- identifier
            _ <- symbol "="
            e <- parseExp
            pure (x, e)
        parseLam = do
            _ <- char '\\'
            x <- identifier
            _ <- symbol "->"
            ExpLam x <$> parseExp
        parseRecord = between (symbol "{") (symbol "}") $
            fmap (ExpRecord . Map.fromList) $
                parseRecordField `sepEndBy1` symbol ","
        parseRecordField = do
            x <- identifier
            _ <- symbol "="
            (x,) <$> parseExp
        parseTyped = between (symbol "(") (symbol ")") $
            ExpTyped <$> (parseExp <* symbol "::") <*> parseTy

parseLit :: Parser Lit
parseLit = label "literal" $
        LitBool True  <$ symbol "True"
    <|> LitBool False <$ symbol "False"
    <|> try (LitFloat <$> floatLiteral)
    <|> try (LitWord64 <$> wordLiteral)
    <|> LitInt64 <$> intLiteral
    <|> LitString . BC.pack <$> stringLiteral
    <|> LitChar <$> charLiteral

parseTy :: Parser Ty
parseTy = label "type" $ foldr1 TyArrow <$> parseTyAtom `sepBy1` symbol "->"



parseTyAtom :: Parser Ty
parseTyAtom =
        TyBool   <$ symbol "Bool"
    <|> TyInt64  <$ symbol "Int"
    <|> TyWord64 <$ symbol "Word"
    <|> TyString <$ symbol "String"
    <|> TyChar   <$ symbol "Char"
    <|> TyFloat  <$ symbol "Float"
    <|> TyVar <$> identifier
    <|> parseTyRecord
    <|> between (symbol "(") (symbol ")") parseTy
    where
        parseTyRecord = between (symbol "{") (symbol "}") $
            fmap (TyRecord . Map.fromList) $
                parseTyRecordField `sepEndBy1` symbol ","
        parseTyRecordField = do
            k <- identifier
            _ <- symbol ":"
            v <- parseTy
            pure (k, v)
