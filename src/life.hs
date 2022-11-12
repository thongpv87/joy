{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Foldable
import GHC.Ix (Ix, range)
import Text.Show.Deriving (deriveShow1)

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Stream a = Cons a (Stream a)
  deriving (Show, Functor)

instance Comonad Stream where
  extract (Cons e _) = e
  duplicate (Cons e as) = Cons (Cons e as) (duplicate as)

instance Foldable Stream where
  foldMap :: Monoid m => (a -> m) -> Stream a -> m
  foldMap f (Cons x xs) = mappend (f x) (foldMap f xs)

data Store s a = Store (s -> a) s
  deriving (Functor)

instance (Show a) => Show (Store s a) where
  show = show . extract

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (Store f) s

newtype Grid s a = Grid (Store s (Store s a))
  deriving (Functor)
  deriving newtype (Show)

ppGrid :: (Show a, Enum s) => Grid s [[a]] -> String
ppGrid = unlines . fmap show . extract

instance (Enum s) => Comonad (Grid s) where
  extract (Grid (Store f s)) = let Store f' _ = f s in f' s
  duplicate :: Grid s a -> Grid s (Grid s a)
  duplicate (Grid (Store f s)) = Grid (Store (Store f') s)
    where
      f' = Grid . Store f

-- useful to show element around the comonad cursor
aroundStoreCenter :: (Ix s, Num s) => s -> Store s a -> Store s [a]
aroundStoreCenter n (Store f s) = Store (\s -> f <$> range (s - n, s + n)) s

aroundGridCenter :: (Ix s, Num s) => s -> Grid s a -> Grid s [[a]]
aroundGridCenter n (Grid (Store f s)) = Grid (Store f1 s)
  where
    f1 s' = Store (\s' -> extract . aroundStoreCenter n . f <$> range (s' - n, s' + n)) s'

main = do
  print $ aroundStoreCenter 5 (Store id 7)
  putStrLn $ ppGrid $ aroundGridCenter 5 (Grid (duplicate (Store id 7)))
  putStrLn "Hello world"
