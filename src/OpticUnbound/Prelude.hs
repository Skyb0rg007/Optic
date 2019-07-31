{-# LANGUAGE OverloadedStrings #-}

module OpticUnbound.Prelude where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc        (pretty)
import           Unbound.Generics.LocallyNameless

import           OpticUnbound.AST
import           OpticUnbound.Evaluate

optLoadPrelude :: OpticExpr -> OpticExpr
optLoadPrelude e = foldl addLet e optPrelude
    where
        addLet :: OpticExpr -> (Text, OpticExpr) -> OpticExpr
        addLet acc (x, e) = Let (bind (rec (s2n $ T.unpack x, Embed e)) acc)

optPrint :: OpticExpr -> Evaluate OpticExpr
optPrint exp = Literal LitUnit <$ liftIO (print $ pretty exp)

optPrelude :: [(Text, OpticExpr)]
optPrelude =
    [ ("print", Primitive $ OpticPrim "print" optPrint)
    , ("nil", DataType "Nil" [])
    , ("cons", Lambda (bind (s2n "x") $ Lambda (bind (s2n "xs") $ DataType "Cons" [Var $ s2n "x", Var $ s2n "xs"])))
    ]
