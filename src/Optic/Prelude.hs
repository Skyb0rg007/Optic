{-# LANGUAGE OverloadedStrings #-}

module Optic.Prelude where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (pretty)

import           Optic.AST                 (OpticExpr (..), OpticLit (..),
                                            OpticPrim (..))
import           Optic.Evaluate            (Environ)

optPrint :: MonadIO m => OpticExpr -> m OpticExpr
optPrint exp = Literal LitUnit <$ liftIO (print $ pretty exp)

optPrelude :: Environ
optPrelude = Map.fromList
    [ ("print", Primitive $ OpticPrim "print" optPrint)
    , ("nil", DataType "Nil" [])
    , ("cons", Lambda "x" $ Lambda "xs" $ DataType "Cons" [Var "x", Var "xs"])
    ]
