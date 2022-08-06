{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module Main where
import Control.Arrow

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

instance Functor BTree where
    fmap _ Nil = Nil
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

x = Node 0 (Node 1 (Node 3 Nil Nil) (Node 4 Nil Nil)) (Node 2 (Node 5 Nil Nil) (Node 6 Nil Nil))

data BTreeF a t = NilF | NodeF a t t
    deriving (Show, Functor)

x' = Fix (NodeF 3 (Fix (NodeF 4 (Fix NilF) (Fix NilF))) (Fix (NodeF 5 (Fix NilF) (Fix NilF))))

y = NodeF 3  (NodeF 4  NilF  NilF)  (NodeF 5  NilF  NilF)

data NatF a = ZeroF | NextF a
    deriving (Show, Functor)

toNat 0 = Fix ZeroF
toNat n = Fix (NextF (toNat (n-1)))

fromNat :: Fix NatF -> Integer
fromNat = cata f
    where f ZeroF = 0
          f (NextF a) = 1 + a

x1 :: Fix NatF
x1 = toNat 5

fib ZeroF = (1, 1)
fib (NextF (m, n)) = (n, m+n)

f2 = cata fib



newtype Fix f = Fix
    { unFix :: f (Fix f)
    }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
-- cata = undefined
cata f = unFix >>> fmap (cata f) >>> f

type CoAlgebra f a = a -> f a

ana :: Functor f => CoAlgebra f a -> a -> Fix f
ana f = Fix <<< fmap (ana f) <<< f

getSum NilF = 0
getSum (NodeF a l r) = a + l * r

data ListF e a = NullF | ConsF e a
    deriving (Show, Functor)

toListF [] = Fix NullF
toListF (x:xs) = Fix (ConsF x (toListF xs))

fromListF :: Fix (ListF a) -> [a]
fromListF = cata f
    where f NullF = []
          f (ConsF x xs) = x:xs

foldListF :: (a->b->b) -> b -> Fix (ListF a) -> b
foldListF f b = cata go
    where go NullF = b
          go (ConsF y ys) = f y ys

-- gen n = NodeF n (BTreeF (n-1)) (BTreeF (n-1))
-- gen 0 = NilF

f NilF = "Nil"
f (NodeF a l r) = unlines [show a, unwords [l, "-", r]]

main :: IO ()
main = do
    -- putStrLn "Hello World"
    -- putStr $ cata (sum)
    (x, y) <- f2 (toNat 10000000) <$ pure ()
    print $ last <$> [show x, show y]
    pure ()
