{-# LANGUAGE OverloadedStrings #-}

module Optic.Prelude where

import           Control.Monad.Except
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc        (pretty)
import           Unbound.Generics.LocallyNameless

import           Optic.AST
import           Optic.Evaluate

optEvalWithEnv :: [OpticPrim] -> OpticExpr -> IO (Either Text OpticExpr)
optEvalWithEnv [] expr = optEval expr
optEvalWithEnv (prim@(OpticPrim name fn) : rest) expr =
    optEvalWithEnv rest (optLet name (Primitive prim) expr)

optPrelude :: [OpticPrim]
optPrelude = [optSucc, optPrint]

optSucc = let succ' (Literal (LitInt n)) = pure $ Literal $ LitInt $ n + 1
              succ' _                    = throwError "succ: wrong argument"
           in OpticPrim "succ" succ'

optPrint = let print' exp = optLitBool True <$ print (pretty exp)
            in OpticPrim "print" (liftIO . print')
