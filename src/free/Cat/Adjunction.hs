{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cat.Adjunction where

import Cat.Category

-- import qualified Prelude as B ((.))

-- class Functor f => Representable f where
--   fmap' :: f a -> (a -> b) -> f b

-- -- | TODO - check f and u constraint
-- class (Functor f, Functor u) => Adjunction f u | f -> u, u -> f where
--   leftAdjunct :: (f a -> b) -> (a -> u b)
--   leftAdjunct f = fmap f B.. unit
--   rightAdjunct :: (a -> u b) -> (f a -> b)
--   rightAdjunct f = counit B.. fmap f

--   unit :: a -> u (f a)
--   unit = leftAdjunct id
--   counit :: f (u a) -> a
--   counit = rightAdjunct id

class (Category m, Category n) => Functor f m n where
  fmap :: (a `m` b) -> a `n` b

-- nat :: (Category m, Category n, Functor f m n, Functor g m n) => f m n -> g m n
type f *-> g = forall m n. (Functor f m n, Functor g m n) => f m n -> g m n
