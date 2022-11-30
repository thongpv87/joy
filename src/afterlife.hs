{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -ddump-deriv
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes #-}

module Main where

import Control.Monad.State

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Store s a = Store
  { peek :: s -> a,
    pos :: s
  }

instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

instance (Show a) => Show (Store s a) where
  show = show . extract

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

instance (Monoid s) => Applicative (Store s) where
  pure a = Store (const a) mempty
  (Store f s) <*> (Store g t) = Store (\s' -> f s' (g s')) (mappend s t)

instance (Monoid s) => Monad (Store s) where
  (>>=) :: Store s a -> (a -> Store s b) -> Store s b
  Store f s >>= fm = Store h b
    where
      h s' = peek (fm (f s')) s'
      b = s `mappend` pos (fm (f mempty))

instance Monoid s => MonadState s (Store s) where
  get :: Store s s
  get = Store id mempty
  put :: s -> Store s ()
  put s = Store (const ()) s

runState :: Store s a -> s -> (a, s)
runState (Store f _) s = (f s, s)

test :: (Monad m) => m a
test = undefined

main = do
  putStrLn "Hello world"
