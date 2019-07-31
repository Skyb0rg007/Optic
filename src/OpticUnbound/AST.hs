{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

{-# OPTIONS_GHC -Wall #-}

module OpticUnbound.AST where

import           Control.Lens
import           Control.Monad.Except             (ExceptT, MonadError)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Function                    (on)
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.Orphans         ()

{-
    The evaluation monad
-}

newtype Evaluate a = Evaluate
    { runEnviron :: ExceptT Text (LFreshMT IO) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadError Text
                       , MonadIO
                       , LFresh
                       )

{-
    The AST
-}

-- Expressions
data OpticExpr
    = Var (Name OpticExpr)
    -- x
    | Annotated OpticExpr OpticType
    -- x : t
    | Lambda (Bind (Name OpticExpr) OpticExpr)
    -- \x -> e
    | App OpticExpr OpticExpr
    -- e1 e2
    | Let (Bind (Rec (Name OpticExpr, Embed OpticExpr)) OpticExpr)
    -- let x = e1 in e2
    | Literal OpticLit
    -- 1 | "str" | ...
    | Primitive OpticPrim
    -- <print>
    | Case OpticExpr (NonEmpty OpticCase)
    -- case e | Cons a b -> f x | Nil -> g () end
    | DataType Text [OpticExpr] -- Cons a b | Nil (etc.)
    -- n/a
    deriving stock (Show, Generic)
    deriving anyclass Alpha

instance Subst OpticExpr OpticExpr where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing

instance Subst OpticExpr OpticType where
    isvar _ = Nothing

instance Eq OpticExpr where
    (==) = aeq

-- Case Block
data OpticCase
    = CaseMatch (Bind OpticPattern OpticExpr)
      -- | patt -> e
    | CaseEff (Bind (OpticPattern, Name Continuation) OpticExpr)
      -- | effect patt k -> e
    deriving stock (Generic, Show)
    deriving anyclass Alpha

instance Subst OpticExpr OpticCase where
    isvar _ = Nothing

-- Continuation (for running effects)
type Continuation = OpticExpr -> Evaluate OpticExpr

-- Pattern
data OpticPattern
    = PatVar (Name OpticExpr)
    | PatCon Text [OpticPattern]
    | PatLit OpticLit
    deriving stock (Generic, Show)
    deriving anyclass Alpha

instance Subst b OpticPattern where
    isvar _ = Nothing

-- Literals
data OpticLit
    = LitInt !Int
    | LitBool !Bool
    | LitText !Text
    | LitChar !Char
    | LitUnit
    deriving stock (Generic, Show)
    deriving anyclass Alpha

instance Subst b OpticLit where
    isvar _ = Nothing

-- Primitive functions
data OpticPrim = OpticPrim
    { _primName :: Text
    , _primFun  :: OpticExpr -> Evaluate OpticExpr
    }

primName :: Lens' OpticPrim Text
primName f (OpticPrim name fn) = fmap (`OpticPrim` fn) (f name)

primFun :: Lens' OpticPrim (OpticExpr -> Evaluate OpticExpr)
primFun f (OpticPrim name fn) = fmap (OpticPrim name) (f fn)

instance Show OpticPrim where
    show (OpticPrim name _) = "<" ++ T.unpack name ++ ">"

instance Eq OpticPrim where
    (==) = (==) `on` _primName

instance Ord OpticPrim where
    compare = compare `on` _primName

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

-- Datatypes
data OpticType
    = TyUnit
    deriving stock (Generic, Show, Eq)
    deriving anyclass Alpha

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
    pretty (CaseMatch b) = runFreshM $ do
        (p, e) <- unbind b
        pure $ "|" <+> pretty p <+> "->" <+> pretty e
    pretty (CaseEff b) = runFreshM $ do
        ((p, k), e) <- unbind b
        pure $ "| effect" <+> parens (pretty p) <+> viaShow k <+> "->" <+> pretty e

instance Pretty OpticPrim where
    pretty = viaShow

instance Pretty OpticPattern where
    pretty (PatLit l)      = pretty l
    pretty (PatVar v)      = viaShow v
    pretty (PatCon c [])   = pretty c
    pretty (PatCon c args) = pretty c <+> hsep (pretty <$> args)

instance Pretty OpticExpr where
    pretty :: OpticExpr -> Doc ann
    pretty expr = runLFreshM $ prettyExpr 0 expr
        where
            parensIf :: Bool -> Doc ann -> Doc ann
            parensIf True  = parens
            parensIf False = id

            prettyExpr :: LFresh m => Int -> OpticExpr -> m (Doc ann)
            prettyExpr prec = \case
                Var v -> pure $ viaShow v
                Annotated e _ -> prettyExpr prec e
                Lambda b -> lunbind b $ \(x, e) -> do
                    e' <- prettyExpr (prec+1) e
                    pure $ parensIf (prec > 0) $ "\\" <> viaShow x <+> "->" <+> e'
                App e1 e2 -> parensIf (prec > 0) .  hsep
                    <$> sequence [prettyExpr (prec+1) e1, prettyExpr prec e2]
                Let b -> lunbind b $ \(r, e2) ->
                    let (x, Embed e1) = unrec r
                     in pure $ parensIf (prec > 0) $
                        "let" <+> group (viaShow x <+> "=" <+> pretty e1) <+> "in" <+> pretty e2
                Literal l -> pure $ pretty l
                Primitive p -> pure $ pretty p
                Case e cases -> pure $
                    "case" <+> pretty e
                    <+> group (hsep . NonEmpty.toList $ pretty <$> cases)
                    <+> "end"
                DataType ty args -> pure $ pretty ty <+> hsep (pretty <$> args)

