{-# LANGUAGE TypeApplications  #-}
module Main where
import Control.Applicative
import Data.Functor

type Time = Double
newtype Behavior a = B (Time -> a)

instance Functor Behavior where
    fmap :: (a -> b) -> Behavior a -> Behavior b
    fmap f (B fa) = B (fmap f fa)

instance Applicative Behavior where
    pure :: a -> Behavior a
    pure = B . const

    (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
    B ff <*> B fa = B (ff <*> fa)

instance (Semigroup a) => Semigroup (Behavior a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Behavior a) where
    mempty = pure mempty

instance Num a => Num (Behavior a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

newtype Event a = E (Behavior [a])

instance Functor Event where
    fmap :: (a -> b) -> Event a -> Event b
    fmap f (E b) = E $ (fmap . fmap) f b

instance Semigroup (Event a) where
    E f <> E g =  E (f <> g)


time :: Behavior Time
time = B id

switcher :: Behavior a -> Event a -> Behavior a
switcher = undefined

sample :: [Time] -> Behavior a -> [a]
sample ts (B f) =
    fmap f ts

sample' ts (E b) =
    sample ts b

evt1 = E $ B $ \t ->
    if 5 <= t && t < 10
    then [t, t+1]
    else []

evt2 = E $ B $ \t ->
    if 6 <= t && t < 9
    then [t, t+1]
    else []

main :: IO ()
main = do
    -- print $ sample' [1..10] (evt1)
    print $ sample' [1..10] (fmap (+1) evt1)
    print $ sample' [1..10] (fmap (+1) evt2)
    print $ sample' [1..10] (evt1 <> evt2)
    test1


test1 = do
    print $ sample [1..10] (pure 4 + pure 5)
    print $ sample [1..10] (negate $ pure 5)
    print $ sample [1..10] (abs $ pure (-5))
    print $ sample [1..10] (fromInteger 10)
    print $ sample [1..10] (fromInteger 7)
    -- print $ sample [1..10] (liftA2 (+) (pure 1) (pure 5))
