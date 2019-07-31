{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Optic.AST where

import           Control.Monad.Except      (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Function             (on)
import           Data.List.NonEmpty        (NonEmpty, toList)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

{-
    The AST
-}

-- Expressions
data OpticExpr
    = Var Text
    | Annotated OpticExpr OpticType
    | Lambda Text OpticExpr
    | App OpticExpr OpticExpr
    | Let Text OpticExpr OpticExpr
    | Literal OpticLit
    | Primitive OpticPrim
    | Case OpticExpr (NonEmpty OpticCase)
    | DataType Text [OpticExpr] -- Cons a b | Nil (etc.)
    deriving (Show, Eq)

-- Case Block
data OpticCase
    = CaseMatch OpticPattern OpticExpr    -- | patt -> e
    | CaseEff OpticPattern Text OpticExpr -- | effect patt k -> e
    deriving (Show, Eq)

-- Pattern
data OpticPattern
    = PatVar Text
    | PatLit OpticLit
    | PatCon Text [OpticPattern]
    deriving (Show, Eq)

-- Literals
data OpticLit
    = LitInt !Int
    | LitBool !Bool
    | LitText !Text
    | LitChar !Char
    | LitUnit
    deriving (Show, Eq)

-- Primitive functions
data OpticPrim = OpticPrim
    { primName :: Text
    , primFun :: forall m . (MonadIO m, MonadError Text m) => OpticExpr -> m OpticExpr
    }

instance Show OpticPrim where
    show (OpticPrim name _) = "<" ++ T.unpack name ++ ">"

instance Eq OpticPrim where
    (==) = (==) `on` primName

-- Datatypes
data OpticType
    = TyUnit
    deriving (Show, Eq)

{-
    Pretty printing
-}

instance Pretty OpticLit where
    pretty (LitInt i)  = pretty i
    pretty (LitBool b) = if b then "True" else "False"
    pretty (LitText t) = pretty t
    pretty (LitChar c) = pretty c
    pretty LitUnit     = "()"

instance Pretty OpticCase where
    pretty (CaseMatch a b) = group $ pretty a <+> "->" <+> pretty b
    pretty (CaseEff a k b) = group $ "effect" <+> pretty a <+> pretty k <+> "->" <+> pretty b

instance Pretty OpticPrim where
    pretty = viaShow

instance Pretty OpticPattern where
    pretty (PatLit l)      = pretty l
    pretty (PatVar v)      = pretty v
    pretty (PatCon c args) = pretty c <+> hsep (pretty <$> args)

instance Pretty OpticExpr where
    pretty = prettyExpr 0
        where
            parensIf :: Bool -> Doc ann -> Doc ann
            parensIf True  = parens
            parensIf False = id

            prettyExpr :: Int -> OpticExpr -> Doc ann
            prettyExpr prec = \case
                Var v -> pretty v
                Annotated e t -> prettyExpr prec e
                Lambda x e -> parensIf (prec > 0) $
                    "\\" <> pretty x <+> "->" <+> prettyExpr (prec+1) e
                App e1 e2 -> parensIf (prec > 0) $
                    prettyExpr (prec+1) e1 <+> prettyExpr prec e2
                Let x e1 e2 -> parensIf (prec > 0) $
                    "let" <+> group (pretty x <+> "=" <+> pretty e1) <+> "in" <+> pretty e2
                Literal l -> pretty l
                Primitive p -> pretty p
                Case e cases ->
                    "case" <+> pretty e <+> "of" <+> group (hsep . toList $ pretty <$> cases)
                DataType ty args -> pretty ty <+> hsep (pretty <$> args)


