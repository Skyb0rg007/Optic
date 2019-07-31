{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module OpticUnbound.Evaluate where

import           Control.Monad.Except             (ExceptT,
                                                   MonadError (throwError),
                                                   runExceptT)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (MonadReader (local), ReaderT,
                                                   asks, runReaderT)
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc        (pretty)
import           Unbound.Generics.LocallyNameless

import           OpticUnbound.AST

optEval :: OpticExpr -> IO (Either Text OpticExpr)
optEval = runLFreshMT . runExceptT . runEnviron . optEval'

optEval' :: OpticExpr -> Evaluate OpticExpr
optEval' = \case
    -- Do nothing
    Lambda b -> pure $ Lambda b
    Literal l -> pure $ Literal l
    Primitive p -> pure $ Primitive p
    Annotated e _ -> optEval' e
    Var v -> pure $ Var v
    -- Var v -> throwError $ "Unable to find " <> T.pack (show v) <> " in the environment"
    -- Evaluate the arguments
    DataType c cs -> DataType c <$> traverse optEval' cs
    App e1 e2 -> do
        e2' <- optEval' e2
        optEval' e1 >>= \case
            Lambda b -> lunbind b $ \(x, e1') ->
                optEval' $ subst x e2' e1'
            Primitive (OpticPrim _ fun) -> fun e2'
            e -> throwError $ "Application of non-lambda term " <> T.pack (show $ pretty e)
    Let b -> lunbind b $ \(r, e2) -> do
        let (x, Embed e1) = unrec r
        e1' <- optEval' e1
        optEval' $ subst x e1' e2
    Case e (c:|cs) -> do
        throwError "NYI"

-- caseMatch :: OpticExpr -> [OpticCase] -> Evaluate OpticExpr
-- caseMatch _ [] = throwError "Could not match!"
-- -- | x -> e
-- caseMatch e (CaseMatch (PatVar v) b:_) = local (Map.insert v e) (optEval' b)
-- -- | 1 -> e
-- caseMatch e (CaseMatch (PatLit l) b:_) | e == Literal l = optEval' b
-- -- | Con a b -> e
-- caseMatch (DataType ty tyArgs) (CaseMatch (PatCon c cArgs) b:_) | c == ty =
    -- if length tyArgs == length cArgs
       -- then throwError "NYI"
       -- else throwError "Wrong number of patterns"
-- caseMatch e (_:rest) = caseMatch e rest
