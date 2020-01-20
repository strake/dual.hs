{-# LANGUAGE PatternSynonyms #-}
module Data.Bifunctor.Flip where

import Control.Category.Dual

type Flip = Dual

pattern Flip :: f a b -> Dual f b a
pattern Flip { flip } = Dual flip
