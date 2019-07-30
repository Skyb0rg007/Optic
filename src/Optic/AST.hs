{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Optic.AST where

import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Function                    (on)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.Orphans         ()

{-
   Helpers
-}

optVar :: Text -> OpticExpr
optVar = Var . s2n . T.unpack

optLambda :: Text -> OpticExpr -> OpticExpr
optLambda x e = Lambda $ bind (s2n $ T.unpack x, Embed TyUnit) e

optApp :: OpticExpr -> OpticExpr -> OpticExpr
optApp = App

optLet :: Text -> OpticExpr -> OpticExpr -> OpticExpr
optLet x e1 e2 = Let e1 $ bind (s2n $ T.unpack x) e2

optPrimPure :: Text -> (OpticExpr -> OpticExpr) -> OpticExpr
optPrimPure name fn = Primitive $ OpticPrim name (pure . fn)

optPrim :: Text -> (forall m . (MonadIO m, Fresh m) => OpticExpr -> m OpticExpr) -> OpticExpr
optPrim name fn = Primitive $ OpticPrim name fn

optLitInt :: Int -> OpticExpr
optLitInt = Literal . LitInt

optLitBool :: Bool -> OpticExpr
optLitBool = Literal . LitBool

optLitText :: Text -> OpticExpr
optLitText = Literal . LitText

optLitChar :: Char -> OpticExpr
optLitChar = Literal . LitChar

{-
   The AST
-}

-- Literal values
data OpticLit
    = LitInt !Int -- 1
    | LitBool !Bool -- true
    | LitText !Text -- "abc"
    | LitChar !Char -- 'c'
    deriving (Generic, Alpha, Show, Eq)

-- Optic expressions
data OpticExpr
    = Var (Name OpticExpr)
        -- x
    | Annotated OpticExpr OpticType
        -- e : t <=> Annotated e t
    | Lambda (Bind (Name OpticExpr, Embed OpticType) OpticExpr)
        -- \x : t -> e <=> Lambda ((x, t) e)
    | App OpticExpr OpticExpr
        -- e1 e2
    | Let OpticExpr (Bind (Name OpticExpr) OpticExpr)
        -- let x = e1 in e2 <=> Let e1 (x, e2)
    | Literal OpticLit
        -- 1 | true | "abc"
    | Primitive OpticPrim
        -- Primitive "succ" (+1) <=> plus
    deriving (Generic, Alpha, Show)

instance Eq OpticExpr where
    (==) = aeq

instance Subst OpticExpr OpticExpr where
    isvar (Var x) = Just (SubstName x)
    isvar _       = Nothing

instance Subst OpticExpr OpticType where
    isvar _ = Nothing

instance Subst OpticExpr OpticLit where
    isvar _ = Nothing

-- Optic primitives
data OpticPrim = OpticPrim
    { primName :: Text
    , primFun  :: forall m . (MonadIO m, Fresh m, MonadError Text m) => OpticExpr -> m OpticExpr
    }

instance Show OpticPrim where
    show (OpticPrim name _) = T.unpack name

instance Eq OpticPrim where
    (==) = (==) `on` primName

instance Ord OpticPrim where
    compare = compare `on` primName

instance Alpha OpticPrim where
    aeq' _ = (==)
    fvAny' _ _ = pure
    close _ _ = id
    open _ _ = id
    isPat _ = mempty
    isTerm _ = mempty
    nthPatFind _ = mempty
    namePatFind _ = mempty
    swaps' _ _p = id
    freshen' _ i = pure (i, mempty)
    lfreshen' _ i cont = cont i mempty
    acompare' _ = compare

instance Subst b OpticPrim where
    isvar _ = Nothing
    subst _ _ = id
    substs _ = id

-- Optic types
data OpticType
    = TyUnit
    deriving (Generic, Alpha, Show, Eq)

{-
   Pretty printers
-}

maybeParens :: Bool -> Doc a -> Doc a
maybeParens True  = parens
maybeParens False = id

instance Pretty OpticLit where
    pretty (LitInt i)  = pretty i
    pretty (LitBool b) = if b then "true" else "false"
    pretty (LitText t) = viaShow t
    pretty (LitChar c) = viaShow c

instance Pretty OpticExpr where
    pretty = runFreshM . prettyExpr 10
        where
        prettyExpr :: Fresh m => Int -> OpticExpr -> m (Doc a)
        prettyExpr prec = \case
            Var v -> pure $ viaShow v
            Annotated e t -> pure $ maybeParens (prec < 10) $
                pretty e <+> viaShow "t"
            Lambda b -> do
                ((x, Embed t), e) <- unbind b
                e' <- prettyExpr 8 e
                pure $ maybeParens (prec < 9) $
                    "\\" <> viaShow x <+> ":" <+> pretty t <+> "->" <+> e'
            App e1 e2 -> (<+>) <$> prettyExpr 8 e1 <*> prettyExpr 8 e2
            Let e1 b -> do
                (x, e2) <- unbind b
                pe1 <- prettyExpr 8 e1
                pe2 <- prettyExpr 8 e2
                pure $ maybeParens (prec < 9) $
                        "let"
                    <+> group (viaShow x <+> "=" <+> pe1)
                    <+> "in" <+> pe2
            Literal l -> pure $ pretty l
            Primitive (OpticPrim name _) -> pure $ "<" <> pretty name <> ">"

instance Pretty OpticType where
    pretty _ = "?"

