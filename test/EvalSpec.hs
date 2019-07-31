{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module EvalSpec ( spec ) where

import           Control.Monad.Except
import           GHC.Stack             (HasCallStack)
import           System.IO.Silently    (capture_)
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Optic.AST
import           Optic.Evaluate

-- Ensure an IO action prints out a specific string
shouldPrint :: HasCallStack => IO a -> String -> Expectation
shouldPrint action expected = capture_ action >>= (`shouldBe` expected)

spec :: Spec
spec = describe "optEval" $ do
    it "constant functions work" $
        optEval (App (Lambda "x" $ Var "x") (Literal $ LitInt 1))
        `shouldReturn` Right (Literal $ LitInt 1)
    it "successor function works" $
        let succ' (Literal (LitInt n)) = pure $ Literal $ LitInt $ succ n
            succ' _                    = throwError "Type error!"
        in
        optEval (App
            (Primitive $ OpticPrim "succ" succ')
            (Literal $ LitInt 1))
        `shouldReturn` Right (Literal $ LitInt 2)
    it "prints in order" $
        let printHello n = App
                (Primitive $ OpticPrim "hello_world" (\_ -> liftIO ((Literal $ LitInt 3) <$ putStrLn ("Hello World! " ++ show n))))
                (Literal $ LitBool True)
        in
        optEval (Let "_" (printHello 1) (Let "_" (printHello 2) (printHello 3)))
        `shouldPrint`
        "Hello World! 1\nHello World! 2\nHello World! 3\n"

