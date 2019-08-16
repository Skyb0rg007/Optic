{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Optic.Core.Eval
    ( optEvaluate
    ) where

import           Bound
import           Control.Monad.Except
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Optic.Core.Expr

tshow :: Show a => a -> Text
tshow = T.pack . show

newtype Evaluate a = Evaluate { runEvaluate :: ExceptT Text IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError Text
             , MonadIO
             )

optEvaluate :: (Show a, Eq a) => OpticExpr a -> IO (Either Text (OpticExpr a))
optEvaluate = runExceptT . runEvaluate . optEval

optEval :: (Show a, Eq a) => OpticExpr a -> Evaluate (OpticExpr a)
optEval = \case
    Var v -> pure $ Var v
    Primitive p -> pure $ Primitive p
    Literal l -> pure $ Literal l
    App f x -> optEval f >>= \case
        Lambda s -> do
            x' <- optEval x
            optEval $ instantiate1 x' s
        Primitive (OpticPrim _ f) -> do
            x' <- optEval x
            optEval =<< f x
        e -> throwError $ "Cannot apply non-lambda: " <> tshow e
    Lambda s -> pure $ Lambda s
    Let bs b ->
        let inst = instantiate (map inst bs !!)
         in optEval $ inst b
    Constructor n args -> Constructor n <$> traverse optEval args
    Case e cases -> optEval e >>= caseEval cases

caseEval :: (Show a, Eq a)
         => [(OpticPat, Scope Int OpticExpr a)]
         -> OpticExpr a
         -> Evaluate (OpticExpr a)
caseEval [] _ = throwError "Could not match any case expressions"
caseEval ((PatWildcard, e):_) _ = optEval (instantiate1 (Literal $ LitString "error!") e)
caseEval ((PatVariable, e):_) e' = optEval (instantiate1 e' e)
caseEval ((PatConstructor n arity, e):rest) (Constructor n' args)
  | n /= n' = caseEval rest (Constructor n' args)
  | arity /= length args = throwError $
      "Cannot match " <> tshow (length args) <> ", expected " <> tshow arity <> "."
  | otherwise = optEval (instantiate (args !!) e)
caseEval _ exp = throwError $ "Case matching on non-constructor " <> tshow exp
