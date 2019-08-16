{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Optic.AST
    ( Lit (..)
    , Ty (..)
    , Exp (..)
    ) where

import           Control.DeepSeq           (NFData)
import           Data.ByteString           (ByteString)
import           Data.Data                 (Data)
import           Data.Int                  (Int64)
import           Data.List                 (intersperse)
import           Data.Map                  (Map)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
-- import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word64)
import           GHC.Generics              (Generic)

-- * Types

data Ty
    = TyBool
    | TyInt64
    | TyWord64
    | TyString
    | TyChar
    | TyFloat
    | TyRecord !(Map Text Ty)
    | TyVar !Text
    | TyArrow !Ty !Ty
    | TyUnion !Ty !Ty
    | TyIntersect !Ty !Ty
    deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- * Expressions

-- | Expression
data Exp
    = ExpLit !Lit
    | ExpApp !Exp !Exp
    | ExpLam !Text !Exp
    | ExpLetVar !Text
    | ExpLamVar !Text
    | ExpRecord !(Map Text Exp)
    | ExpLet ![(Text, Exp)] !Exp
    | ExpTyped !Exp !Ty
    | ExpGetField !Exp !Text
    deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- | Literals
data Lit
    = LitInt64  !Int64
    | LitWord64 !Word64
    | LitBool   !Bool
    | LitString !ByteString
    | LitChar   !Char
    | LitFloat  !Double
    deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- Pretty printing

instance Pretty Exp where
    pretty = \case
        ExpLit l -> pretty l
        ExpApp f x -> pretty f <+> "(" <> pretty x <> ")"
        ExpLam x e -> "\\" <> pretty x <+> "->" <+> pretty e
        ExpLamVar v -> pretty v
        ExpLetVar v -> "^" <> pretty v
        ExpRecord r ->
            "{" <+> hsep (intersperse "," (prettyField <$> Map.toList r)) <+> "}"
        ExpLet bs e ->
                "let" <+> hsep (intersperse ";" (prettyField <$> bs))
            <+> "in" <+> pretty e
            <+> "end"
        ExpTyped e t -> "(" <> pretty e <+> "::" <+> pretty t <> ")"
        ExpGetField e f -> pretty e <> "." <> pretty f
        where
            prettyField :: (Text, Exp) -> Doc ann
            prettyField (k, v) = pretty k <+> "=" <+> pretty v

instance Pretty Ty where
    pretty = \case
        TyBool      -> "Bool"
        TyInt64     -> "Int"
        TyWord64    -> "Word"
        TyString    -> "String"
        TyChar      -> "Char"
        TyFloat     -> "Float"
        TyRecord r  ->
            "{" <+> hsep (intersperse "," (prettyField <$> Map.toList r)) <+> "}"
        TyVar v     -> pretty v
        TyArrow a b -> "(" <> pretty a <+> "->" <+> pretty b <> ")"
        TyUnion a b -> pretty a <+> "|" <+> pretty b
        TyIntersect a b -> pretty a <+> "&" <+> pretty b
        where
            prettyField :: (Text, Ty) -> Doc ann
            prettyField (k, v) = pretty k <+> ":" <+> pretty v

instance Pretty Lit where
    pretty = \case
        LitInt64 i  -> pretty i
        LitWord64 w -> pretty w <> "u"
        LitBool b   -> pretty b
        LitString s -> viaShow s
        LitChar c   -> pretty c
        LitFloat f  -> pretty f
