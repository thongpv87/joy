{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cat.Functor where

import Cat.Category

class (Category m, Category n) => Functor f m n where
  fmap :: (a `m` b) -> a `n` b

type f *-> g = forall m n. (Functor f m n, Functor g m n) => f m n -> g m n

class Category k => Identity k where
  runIden :: a `k` a
