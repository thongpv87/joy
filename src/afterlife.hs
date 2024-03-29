{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
  { peek :: s -> a
  , pos :: s
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

newtype R a = R a
  deriving (Show, Functor)

instance Applicative R where
  pure = R
  R f <*> R a = R (f a)

instance Monad R where
  R ma >>= f = f ma

mm :: Integer -> R [Integer]
mm _ = pure [1 .. 12]

dd :: Integer -> R [Integer]
dd _ = pure [1 .. 31]

main = do
  print $ do
    y <- pure @R 2022
    m <- mm y
    mapM dd m

  putStrLn "Hello world"
