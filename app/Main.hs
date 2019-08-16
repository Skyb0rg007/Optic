{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad      (when)
import qualified Data.Map           as Map
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
-- import           Data.Text.Prettyprint.Doc (pretty)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           System.IO          (hPrint, hPutStrLn, stderr)
import           Text.Megaparsec    (eof, errorBundlePretty, parse)

import           Optic.Core.Expr
import           Optic.Core.Eval
import           Optic.Core.Parse
import           Optic.Core.Prelude

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1)
        usage
    let str = head args
    case parse (parseExpr <* eof) "<cmdline>" (T.pack str) of
      Left err -> hPutStrLn stderr $ errorBundlePretty err
      Right expr -> do
          putStr "AST:\n\t"
          print expr
          optEvaluate (loadPrelude expr) >>= \case
            Left err -> TIO.hPutStrLn stderr err
            Right expr -> do
                putStr "\nEvaluated AST:\n\t"
                print expr
                -- putStr "Evaluated (Pretty):\n\t"
                -- print $ pretty expr

usage :: IO ()
usage = do
    name <- getProgName
    hPutStrLn stderr $ "Usage: " ++ name ++ " \"str\""
    exitFailure
