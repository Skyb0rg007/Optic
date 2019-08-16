{-# LANGUAGE OverloadedStrings #-}

module Optic.Core.Prelude
    ( loadPrelude
    ) where

import Data.Text (Text)
import Bound
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (($>))

import Optic.Core.Expr

loadPrelude :: OpticExpr Text -> OpticExpr Text
loadPrelude = go prelude
    where
        go [] = id
        go ((name, expr):rest) = substitute name expr . go rest

prelude :: [(Text, OpticExpr Text)]
prelude = 
    [ ("cons", lam "x" (lam "y" (Constructor 0 [Var "x", Var "y"])))
    , ("nil", Constructor 1 [])
    , ("flip", lam "x" (lam "y" (App (Var "y") (Var "x"))))
    , ("print", Primitive (OpticPrim "print" printfn))
    ]
    where
        printfn (Literal l) = liftIO $ print l $> Literal LitUnit
        printfn (Constructor n args) = liftIO $ putStrLn ("Constructor " ++ show n) $> Literal LitUnit
        printfn _ = liftIO $ putStrLn "<thing>" $> Literal LitUnit
