module Main where

import Control.Monad
import Data.List (scanl', unfoldr)

type Loc = (Double, Double)
newtype Image a = Image (Loc -> a)
type Size = (Integer, Integer)

type Month = Integer
type Year = Integer
type Day = Integer
type Week = [Maybe Day]

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    duplicate = extend id
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate

instance Comonad ((,) e) where
    extract = snd
    duplicate (e, a) = (e, (e, a))

newtype Fix f = Fix {unFix :: f (Fix f)}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana alg = Fix . fmap (ana alg) . alg

toL = cata (uncurry (:))

-- findNb :: Integer -> Integer
findNb vol =
    head . dropWhile ((> 0) . snd) $ iterate (\(n, v) -> (n + 1, v - n ^ 3)) (0, vol)

-- toL $ ana (\(n, v) -> if v <= n ^ 3 then (n, (n, v)) else (n + 1, (n + 1, v - n ^ 3))) (0, vol)

findNb' x = let l = takeWhile (<= x) $ scanl' (+) 0 $ fmap (^ 3) [1 ..] in if last l /= x then -1 else length l

main = do
    putStrLn "Hello world"
    print $ findNb' 1071225
    print $ do
        x <- [1 .. 5]
        y <- [1 .. x]
        -- z <- [1 .. x]
        pure (x, y)

-- guard (y ^ 2 + z ^ 2 == x ^ 2)
-- pure (x, y, z)
