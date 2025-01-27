{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where
import Text.Show.Deriving (deriveShow1)
import GHC.Generics         (Generic)
import Data.Monoid
import Data.List
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1, compare1, eq1, readsPrec1, showsPrec1)
import Data.Functor.Contravariant
import Data.Bifunctor
import Control.Applicative

newtype Fix f = Fix {unFix :: f (Fix f)}
  deriving (Generic)

instance Show1 f => Show (Fix f) where
    showsPrec d (Fix a) =
        showParen (d >= 11)
            $ showString "Fix "
            . showsPrec1 11 a

hoistFix :: Functor f => (forall a . f a -> g a) -> Fix f -> Fix g
hoistFix nt = go
  where go (Fix f) = Fix (nt (fmap go f))

hoistFix' :: Functor g => (forall a . f a -> g a) -> Fix f -> Fix g
hoistFix' nt = go
  where go (Fix f) = Fix (fmap go (nt f))


cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana alg = Fix . fmap (ana alg) . alg

data StreamF a b = StreamF a b
  deriving (Functor, Show)

$(deriveShow1 ''StreamF)

era :: Integral a => Fix (StreamF a)
era = ana (\(x : xs) -> StreamF x (filter (\n -> (n `mod` x) /= 0) xs)) [2..]

streamF2List :: Fix (StreamF a) -> [a]
streamF2List = cata (\(StreamF a b) -> a : b)


data RingF a
  = ZeroF
  | OneF
  | AddF a a
  | MultF a a
  | NegF a
  deriving (Functor, Eq)

zero = Fix ZeroF
one = Fix OneF
add a b = Fix (AddF a b)
mult a b = Fix (MultF a b)
neg a = Fix (NegF a)

evalRingF :: Num a => (RingF a -> a)
evalRingF ZeroF = 0
evalRingF OneF = 1
evalRingF (AddF a b) = a + b
evalRingF (MultF a b) = a * b
evalRingF (NegF a) = - a

combineList :: Semigroup a => [a] -> [a] -> [a]
combineList [] ys = ys
combineList xs [] = xs
combineList (x : xs) (y : ys) = (x <> y) : combineList xs ys

newtype Coefficient a = Coefficient { coeff :: (a, Integer) } deriving newtype (Show, Eq)

multCoeff  (Coefficient (a, n)) (Coefficient (b, m)) = Coefficient (a * b, m + n)


evalPolyF :: (Num a, Eq a) => [a] -> (RingF [a] -> [a])
evalPolyF _ ZeroF = [0]
evalPolyF var OneF = var
evalPolyF _ (AddF a b)
  | a == [0] = b
  | b == [0] = a
  | otherwise = getSum <$> combineList (Sum <$> a) (Sum <$> b)
evalPolyF _ (MultF a b)
  | a == [0] = [0]
  | b == [0] = [0]
  | a == [1] = b
  | b == [1] = a
  | otherwise =
    fmap (sum . fmap fst)
    $ groupBy (\x y -> snd x == snd y)
    $ sortOn snd
    $ liftA2 (\(x, m) (y, n) -> (x * y, m + n)) (zip a [0..]) (zip b [0..])

evalPolyF _ (NegF a) = fmap (* (-1)) a

eval :: Fix RingF -> Integer
eval = cata evalRingF

-- (2x + 1) * (x + 2) + (2x + 3) * (x^2 + 2)
-- = 2x^2 + 5x + 2 + 2x^3 + 3x^2 + 4x + 6 = 2x^3 + 5x^2 + 9x + 8 = [8, 9, 5, 2]
expr = (cst' [1, 2] `mult'` cst' [2, 1]) `add'` (cst' [3, 2] `mult'` cst' [2, 0, 1]) `mult'` cst' [-1]

main = do
  -- print $ (streamF2List $ era @Integer) !! 1000
  print $ cata (evalExprF @Integer) expr
  putStrLn "Hello world"

data Expr v a
  = Cst v
  | Add a a
  | Mult a a
  | Neg a
  deriving (Functor, Eq, Applicative)

cst' v = Fix (Cst v)
add' a b = Fix (Add a b)
mult' a b = Fix (Mult a b)
neg' a = Fix (Neg a)


evalExprF :: (Num a, Eq a) => (Expr [a] [a] -> [a])
evalExprF (Cst v) = v
evalExprF (Add a b)
  | a == [0] = b
  | b == [0] = a
  | otherwise = getSum <$> combineList (Sum <$> a) (Sum <$> b)
evalExprF (Mult a b)
  | a == [0] = [0]
  | b == [0] = [0]
  | a == [1] = b
  | b == [1] = a
  | otherwise =
    fmap (sum . fmap fst)
    $ groupBy (\x y -> snd x == snd y)
    $ sortOn snd
    $ liftA2 (\(x, m) (y, n) -> (x * y, m + n)) (zip a [0..]) (zip b [0..])
evalExprF (Neg a) = fmap (* (-1)) a
