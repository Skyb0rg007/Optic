{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Optic.Core.Expr
    ( OpticExpr (..)
    , OpticPat (..)
    , OpticLit (..)
    , OpticPrim (..)
    , lam
    , let_
    ) where

import           Bound
import           Control.Monad                (ap)
import           Control.Monad.Except
import           Data.Function                (on)
import           Data.Functor.Classes         (Eq1 (..), Ord1 (..), Show1 (..),
                                               compare1, eq1, showsPrec1)
import           Data.Functor.Classes.Generic
import           Data.List                    (elemIndex)
import           Data.Text                    (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                 (Generic, Generic1)

-- | A primitive function
data OpticPrim = OpticPrim
    { primName :: Text
    , primFn :: forall m a . (MonadIO m, MonadError Text m)
             => OpticExpr a
             -> m (OpticExpr a)
    }

-- | An expression
-- Note: `Constructor n args` is the nth constructor with the args
data OpticExpr a
    = Var a
    | App (OpticExpr a) (OpticExpr a)
    | Lambda (Scope () OpticExpr a)
    | Let [Scope Int OpticExpr a] (Scope Int OpticExpr a)
    | Constructor !Int ![OpticExpr a]
    | Case (OpticExpr a) [(OpticPat, Scope Int OpticExpr a)]
    | Literal OpticLit
    | Primitive OpticPrim
    deriving (Generic, Generic1, Functor, Traversable, Foldable)

lam :: Text -> OpticExpr Text -> OpticExpr Text
lam x = Lambda . abstract1 x

let_ :: [(Text, OpticExpr Text)] -> OpticExpr Text -> OpticExpr Text
let_ bs b = Let (abstr <$> binds) (abstr b)
    where
        names :: [Text]
        names = fst <$> bs
        binds :: [OpticExpr Text]
        binds = snd <$> bs
        abstr :: OpticExpr Text -> Scope Int OpticExpr Text
        abstr = abstract (`elemIndex` names)

-- | A pattern for case expressions
data OpticPat
    = PatWildcard              -- < Matches anything without binding (_)
    | PatVariable              -- < Matches a single thing (x)
    | PatConstructor !Int !Int -- < Matches the constructor n with m args
    deriving (Generic, Eq, Ord, Show, Read)

-- | A literal, primitive value
data OpticLit
    = LitInt !Int     -- < An integer (3)
    | LitChar !Char   -- < A character ('a')
    | LitString !Text -- < A string ("abcd")
    | LitBool !Bool   -- < A boolean (True)
    | LitUnit         -- < A unit ()
    deriving (Generic, Eq, Ord, Show, Read)

-----------------------------------------------------------------------------
--
-- Instances
--
-----------------------------------------------------------------------------

instance Show OpticPrim where
    showsPrec p (OpticPrim n _) = showsPrec p n
instance Eq OpticPrim where
    (==) = (==) `on` primName
instance Ord OpticPrim where
    compare = compare `on` primName


instance Applicative OpticExpr where
    pure = Var
    (<*>) = ap

instance Monad OpticExpr where
    return = pure
    Var a >>= f = f a
    App a b >>= f = App (a >>= f) (b >>= f)
    Lambda e >>= f = Lambda (e >>>= f)
    Let bs e >>= f = Let (fmap (>>>= f) bs) (e >>>= f)
    Constructor n args >>= f = Constructor n (fmap (>>= f) args)
    Case e cases >>= f = Case (e >>= f) ((fmap . fmap) (>>>= f) cases)
    Literal l >>= _ = Literal l
    Primitive p >>= _ = Primitive p

instance Eq1 OpticExpr where liftEq = liftEqDefault
instance Ord1 OpticExpr where liftCompare = liftCompareDefault
instance Show1 OpticExpr where liftShowsPrec = liftShowsPrecDefault
instance Eq a => Eq (OpticExpr a) where (==) = eq1
instance Ord a => Ord (OpticExpr a) where compare = compare1
instance Show a => Show (OpticExpr a) where showsPrec = showsPrec1

-----------------------------------------------------------------------------
--
-- Pretty-printing
--
-----------------------------------------------------------------------------

-- TODO

