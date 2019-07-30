{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module EvalSpec ( spec ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           GHC.Stack
import           System.IO.Silently
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Optic.AST
import           Optic.Evaluate

shouldPrint :: HasCallStack => IO a -> String -> Expectation
shouldPrint action expected = capture_ action >>= (`shouldBe` expected)

spec :: Spec
spec = describe "optEval" $ do
    it "constant functions work" $
        optEval (optApp (optLambda "x" $ optVar "x") (optLitInt 1))
        `shouldReturn` Right (optLitInt 1)
    it "successor function works" $
        optEval (optApp
            (optPrimPure "succ" (\(Literal (LitInt n)) -> Literal (LitInt $ succ n)))
            (optLitInt 1))
        `shouldReturn` Right (optLitInt 2)
    it "prints in order" $
        let printHello n = optApp
                (optPrim "hello_world" (\_ -> liftIO (optLitInt 3 <$ putStrLn ("Hello World! " ++ show n))))
                (optLitBool True)
        in
        optEval (optLet "_" (printHello 1) (optLet "_" (printHello 2) (printHello 3)))
        `shouldPrint`
        "Hello World! 1\nHello World! 2\nHello World! 3\n"

