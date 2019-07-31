{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unbound.Generics.Orphans () where

import           Data.List.NonEmpty               (NonEmpty)
import           Data.Text                        (Text)
import           Unbound.Generics.LocallyNameless (Alpha (..), Subst (..))

instance Alpha Text where
    aeq' _ = (==)
    fvAny' _ _ = pure
    close _ _ = id
    open _ _ = id
    isPat _ = mempty
    isTerm _ = mempty
    nthPatFind _ = mempty
    namePatFind _ = mempty
    swaps' _ _p = id
    freshen' _ i = pure (i, mempty)
    lfreshen' _ i cont = cont i mempty
    acompare' _ = compare

instance Subst b Text where
    isvar _ = Nothing
    subst _ _ = id
    substs _ = id

-- NonEmpty
instance Alpha a => Alpha (NonEmpty a)
instance Subst c a => Subst c (NonEmpty a)

