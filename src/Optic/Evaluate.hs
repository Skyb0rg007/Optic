{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Optic.Evaluate where

import           Control.Monad.Except   (ExceptT, MonadError (throwError),
                                         runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader (local), ReaderT, asks,
                                         runReaderT)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           Optic.AST

type Environ = Map Text OpticExpr

newtype Evaluate a = Evaluate
    { runEnviron :: ExceptT Text (ReaderT Environ IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadError Text
               , MonadReader Environ
               , MonadIO
               )

optEval :: OpticExpr -> IO (Either Text OpticExpr)
optEval = optEvalWithEnv mempty

optEvalWithEnv :: Environ -> OpticExpr -> IO (Either Text OpticExpr)
optEvalWithEnv env = flip runReaderT env . runExceptT . runEnviron . optEval'

optEval' :: OpticExpr -> Evaluate OpticExpr
optEval' = \case
    Lambda x e -> pure $ Lambda x e
    Literal l -> pure $ Literal l
    Primitive p -> pure $ Primitive p
    DataType c cs -> DataType c <$> traverse optEval' cs
    Var v -> asks (Map.lookup v) >>= \case
        Nothing -> throwError $ "Unable to find " <> v <> " in the environment"
        Just val -> pure val
    Annotated e _ -> optEval' e
    App e1 e2 -> do -- TODO: alpha renaming
        e2' <- optEval' e2
        optEval' e1 >>= \case
            Lambda x e1' -> local (Map.insert x e2') (optEval' e1')
            Primitive (OpticPrim _ fun) -> fun e2'
            _ -> throwError "Application of non-lambda term"
    Let x e1 e2 -> do
        e1' <- optEval' e1
        local (Map.insert x e1') (optEval' e2)
    Case e (c:|cs) -> do
        e' <- optEval' e
        caseMatch e' (c:cs)

caseMatch :: OpticExpr -> [OpticCase] -> Evaluate OpticExpr
caseMatch _ [] = throwError "Could not match!"
-- | x -> e
caseMatch e (CaseMatch (PatVar v) b:_) = local (Map.insert v e) (optEval' b)
-- | 1 -> e
caseMatch e (CaseMatch (PatLit l) b:_) | e == Literal l = optEval' b
-- | Con a b -> e
caseMatch (DataType ty tyArgs) (CaseMatch (PatCon c cArgs) b:_) | c == ty =
    if length tyArgs == length cArgs
       then throwError "NYI"
       else throwError "Wrong number of patterns"
caseMatch e (_:rest) = caseMatch e rest
