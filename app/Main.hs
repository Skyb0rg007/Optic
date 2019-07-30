{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad             (when)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Text.Prettyprint.Doc
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import           System.IO                 (hPrint, hPutStrLn, stderr)
import           Text.Megaparsec

import           Optic.Parser              (parseExpr)
import           Optic.Prelude             (optEvalWithEnv, optPrelude)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1)
        usage
    let str = head args
    case parse parseExpr "<cmdline>" (T.pack str) of
      Left err -> hPutStrLn stderr $ errorBundlePretty err
      Right expr -> do
          print $ pretty expr
          optEvalWithEnv optPrelude expr >>= \case
            Left err -> TIO.hPutStrLn stderr err
            Right expr -> print $ pretty expr

usage :: IO ()
usage = do
    name <- getProgName
    hPutStrLn stderr $ "Usage: " ++ name ++ " \"str\""
    exitFailure
