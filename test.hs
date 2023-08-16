{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.List (genericLength, genericTake, sort)
import Prelude hiding (Maybe (..))

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Show, Functor)

data Maybe a = Nothing | Just a
  deriving (Show, Functor)

data List a = Nil | Cons a (List a)
  deriving (Show, Functor)

type Func a = Int -> a

alpha :: List a -> Tree a
alpha (Cons a _) = Node a Leaf Leaf

beta :: Func a -> Maybe a
beta f = Just (f 5)

alphabet :: Func (List a) -> Maybe (Tree a)
alphabet = fmap alpha . beta

alphabet2 = beta . fmap alpha

f :: Int -> List Int
f 5 = Cons 5 (Cons 6 (Cons 7 Nil))

ff :: List a -> Tree a
ff (Cons a _) = Node a Leaf Leaf

ff' :: List a -> Tree a
ff' (Cons a _) = Node a (Node a Leaf Leaf) Leaf

reverseNumber :: Integer -> [Integer]
reverseNumber seed =
  (seed * (10 ^ l) + rseed) : fmap (\x -> (seed * 10 + x) * (10 ^ l) + rseed) [0 .. 9]
 where
  l = genericLength (show seed)
  rseed = read $ reverse (show seed)

main = do
  print $ (sort $ genericTake (1 * 1000) $ ([0 .. 9] ++ concat (fmap reverseNumber [1 ..])))

-- print $ alphabet f
-- print $ alphabet2 f
