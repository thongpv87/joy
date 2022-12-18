{-# LANGUAGE FunctionalDependencies #-}

module Cat.Adjunction where

class Functor f => Representable f where
  fmap' :: f a -> (a -> b) -> f b

class (Functor f, Representable u) => Adjunction f u | f -> u, u -> f where
  unit :: a -> u (f a)
  unit = undefined
  counit :: f (u a) -> a
  counit = undefined
  leftAdjunct :: (f a -> b) -> (a -> u b)
  leftAdjunct = undefined
  rightAdjunct :: (a -> u b) -> (f a -> b)
  rightAdjunct = undefined
