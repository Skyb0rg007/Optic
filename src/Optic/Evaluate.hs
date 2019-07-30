{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Optic.Evaluate where

import           Control.Monad.Except
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           Unbound.Generics.LocallyNameless (Fresh, FreshMT, bind,
                                                   runFreshMT, subst, unbind)
import           Unbound.Generics.Orphans         ()

import           Optic.AST

newtype Environ a = Environ { runEnviron :: ExceptT Text (FreshMT IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , Fresh           -- for `unbind`
             , MonadError Text -- for throwing eval errors
             , MonadIO         -- for performing IO
             )

-- Run a computation, returning Left err on error, or Right val on success
optEval :: OpticExpr -> IO (Either Text OpticExpr)
optEval = runFreshMT . runExceptT . runEnviron . optEval'

optEval' :: OpticExpr -> Environ OpticExpr
optEval' = \case
    Var v -> pure $ Var v
    Annotated e t -> pure e
    Lambda b -> pure $ Lambda b
    App e1 e2 -> do
        -- match e1 with \x -> e1', then evaluate e2[x/e1']
        e2' <- optEval' e2
        optEval' e1 >>= \case
          Lambda b1 -> do
              ((x1, _), e1') <- unbind b1
              optEval' $ subst x1 e2' e1'
          Primitive prim ->
              optEval' =<< primFun prim e2'
          e -> throwError $ "Application of non-lambda term \"" <> T.pack (show e) <> "\""
    Let e1 b -> do
        -- Evaluate e1, then evaluate e2[x/e1]
        e1' <- optEval' e1
        (x, e2) <- unbind b
        optEval' $ subst x e1' e2
    Literal l -> pure $ Literal l
    Primitive p -> pure $ Primitive p

