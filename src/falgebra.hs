{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Fix (Fix (..), ana, cata)
import Data.Functor.Foldable hiding (ana, cata)
import Data.Functor.Foldable.TH
import Data.List (unfoldr)
import Text.Show.Deriving (deriveShow1)

data PolyF a f
  = ZeroF
  | OneF
  | AddF a f
  | MultF a f
  | NegF a
  deriving (Functor, Show)

$(deriveShow1 ''PolyF)

data Expr
  = EZero
  | EOne
  | EAdd Expr Expr
  | EMult Expr Expr
  | ENeg Expr
  deriving (Show)

data ExprF a
  = EZeroF
  | EOneF
  | EAddF a a
  | EMultF a a
  | ENegF a
  deriving (Functor, Foldable, Traversable)

type instance Base Expr = ExprF

instance Recursive Expr where
  project EZero = EZeroF
  project EOne = EOneF
  project (EAdd a b) = (EAddF a) b
  project (EMult a b) = (EMultF a) b
  project (ENeg a) = ENegF a

instance Corecursive Expr where
  embed EZeroF = EZero
  embed EOneF = EOne
  embed (EAddF a b) = (EAdd a) b
  embed (EMultF a b) = (EMult a) b
  embed (ENegF a) = ENeg a

data NatF a
  = NZero
  | NSuccF a
  deriving (Show, Functor)

$(deriveShow1 ''NatF)

fib :: Fix NatF -> (Integer, Integer)
fib = cata $ \case
  NZero -> (1, 1)
  NSuccF (a, b) -> (b, a + b)

intToNatF :: Integer -> Fix NatF
intToNatF = ana $ \case
  0 -> NZero
  n -> NSuccF (n - 1)

data StreamF e a = StreamF e a
  deriving (Show, Functor)

$(deriveShow1 ''StreamF)

primes :: Fix (StreamF Integer)
primes = ana era [2 ..]
  where
    era (x : xs) = StreamF x (filter (notdiv x) xs)
    notdiv a b = b `mod` a /= 0

streamToList :: Fix (StreamF a) -> [a]
streamToList = cata (\(StreamF a b) -> a : b)

fib2 :: Integer -> (Integer, Integer, Integer)
fib2 n = foldr (\n (_, a, b) -> (n, b, a + b)) (1, 1, 1) [n, n -1 .. 0]

maybeCoalg :: Integer -> Maybe Integer
maybeCoalg 20 = Nothing
maybeCoalg n = Just $ n + 1

squares = ana f 1
  where
    f n = StreamF (n * n) (n + 1)

primes2 = unfoldr (\(x : xs) -> Just (x, filter (notdiv x) xs)) [2 ..]
  where
    notdiv x y = y `mod` x /= 0

toListF :: [a] -> Fix (ListF a)
toListF = ana $ \case
  [] -> Nil
  (x : xs) -> Cons x xs

evalPoly :: (Floating a) => a -> Fix (ListF a) -> (a, a)
evalPoly x =
  cata $ \case
    Nil -> (0, 0)
    Cons a (s, n) -> (s + a * (x ** n), n + 1)

-- multi vars polynomial can be represent as list of its terms
-- Each term can be represent by a pair of (coefficient, [degrees])
-- For examples: x^2 * y - 3 y^3 * z ~ [ [(1, [2, 1, 0]), (3, [0, 3, 1])] ]
evalMultiVarPoly :: Floating a => [a] -> Fix (ListF (a, [a])) -> a
evalMultiVarPoly vars =
  cata $ \case
    Nil -> 0
    Cons (a, ns) s -> s + a * sum (zipWith (**) vars ns)

exampleMultPoly = toListF [(1, [2, 1, 0]), (-3, [0, 3, 1])]

main :: IO ()
main = do
  print $ ("primes: ", take 10 . drop 10000 . streamToList $ primes)
  print $ ("evalPoly: ", evalPoly 3 $ toListF [2, 2, 3])
  print $ ("evalMultiVarPoly: ", evalMultiVarPoly [3, 4, 5] exampleMultPoly)
  print $ ("primes2: ", take 10 primes2)
  print $ ("squares: ", take 10 . streamToList $ squares)

testFib = do
  let x = 10000
  print $ snd . fib $ intToNatF x
  print $ fib2 x
