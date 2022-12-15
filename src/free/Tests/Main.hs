{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Cat.Free
import Control.Monad (ap)

data Pair a = Pair a a
  deriving (Functor, Show)

data Tree a = Bin (Tree a) (Tree a) | Tip a
  deriving (Show, Functor)

instance Applicative Tree where
  pure = Tip
  (<*>) = ap

instance Monad Tree where
  Tip a >>= f = f a
  Bin l r >>= f = Bin (l >>= f) (r >>= f)

x = Pair (Bin (Tip 3) (Tip 2)) (Bin (Tip 4) (Tip 5))

main = do
  print x
  -- print fx
  putStrLn "Hello world"
