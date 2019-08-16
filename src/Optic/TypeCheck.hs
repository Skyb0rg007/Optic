{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Optic.TypeCheck
    (
    ) where

import           Control.DeepSeq           (NFData)
import           Control.Monad             (when)
import           Control.Monad.Except      (MonadError (..))
import           Control.Monad.State       (MonadState (..), modify)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Data                 (Data)
import           Data.Int                  (Int64)
import           Data.Map                  (Map)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (pretty)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word64)
import           GHC.Generics              (Generic)

import           Optic.AST

-- t+
data TyPos
    = TyPosBool -- Bool
    | TyPosArrow !TyNeg !TyPos -- t- -> t+
    | TyPosRecord !(Map Text TyPos) -- { l : t+ }
    | TyPosVar !Text -- a
    | TyPosUnion !TyPos !TyPos -- a | b
    | TyPosBotton -- _|_
    | TyPosScheme !Text !TyPos -- µα.τ+
    deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- t-
data TyNeg
    = TyNegBool -- Bool
    | TyNegArrow !TyPos !TyNeg -- t+ -> t-
    | TyNegRecord !(Map Text TyNeg) -- { l : t- }
    | TyNegVar !Text -- a
    | TyNegIntersect !TyNeg !TyNeg -- a & b
    | TyNegTop -- T
    | TyNegScheme !Text !TyNeg -- µα.τ-
    deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- [Delta-] t+
data TyScheme = TyScheme (Map Text TyNeg) TyPos
    deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- Delta-
-- Pi ==> Pi(x) = [Delta-] t+
data TyEnv = TyEnv
    { tyEnvMono :: Map Text TyNeg    -- Delta
    , tyEnvPoly :: Map Text TyScheme -- Pi
    } deriving (Generic, Data, Typeable, Eq, Ord, Show, NFData)

-- When looking up the type of a lambda argument, if it isn't used then
-- the function takes any type as argument (the Top type)
lookupMono :: Text -> Map Text TyNeg -> TyNeg
lookupMono = Map.findWithDefault TyNegTop

principalScheme :: MonadError Text m
                => TyEnv
                -> Exp
                -> m TyScheme
principalScheme env@TyEnv{tyEnvMono, tyEnvPoly} = \case
    -- Variables
    ExpLetVar v | Just s <- Map.lookup v tyEnvPoly ->
        -- P(Pi, ^x) = Pi(x)
        pure s
    ExpLetVar v -> throwError $ "Could not find variable '" <> v <> "' in the environment"
    ExpLamVar v ->
        -- P(Pi, x) = [x : a] a
        pure $ TyScheme (Map.singleton v (TyNegVar "a")) (TyPosVar "a")

    -- Let expressions
    ExpLet [(v, e1)] e2 -> do
        s1@(TyScheme delta1 _) <- principalScheme env e1
        let extendedEnv = env { tyEnvPoly = Map.insert v s1 tyEnvPoly }
        TyScheme delta2 t2 <- principalScheme extendedEnv e2
        pure $ TyScheme (Map.union delta1 delta2) t2
    ExpLet _ _ -> throwError "Multiple let bindings are not yet supported"

    -- Literals
    ExpLit (LitBool _) -> pure $ TyScheme Map.empty TyPosBool
    ExpLit l -> throwError $ "The literal '" <> T.pack (show (pretty l)) <> "' is not yet implemented"

    -- Lambdas
    ExpLam x e -> do
        TyScheme delta ty <- principalScheme env e
        let xTy = lookupMono x delta
        pure $ TyScheme (Map.delete x delta) (TyPosArrow xTy ty)

    -- Note: hard cases coming up

    _ -> undefined


merge = undefined

biunify :: (MonadState [(TyPos, TyNeg)] m, MonadError Text m)
        => (TyPos, TyNeg)
        -> m ()
biunify tys@(tyPos, tyNeg) = do
    st <- get
    when (tys `elem` st) $
        modify (tys :)
    undefined
