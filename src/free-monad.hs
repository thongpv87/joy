{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (liftA2)
import Control.Monad (ap, filterM, join, when)
import Data.List (intercalate)

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Free g) = Free (fmap f <$> g)

instance Functor f => Applicative (Free f) where
    pure = Pure
    (<*>) = ap

instance Functor f => Monad (Free f) where
    Pure a >>= f = f a
    Free f >>= g = Free (fmap (>>= g) f)

liftF :: Functor f => f a -> Free f a
liftF f = Free (Pure <$> f)

foldFree :: (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree f (Free g) = f g >>= foldFree f -- eq impl: join $ f (fmap (foldFree f) g)

iter :: (Functor f) => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter alg (Free f) = alg (fmap (iter alg) f)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p = foldr f (pure [])
  where
    f a ma = liftA2 (\b -> if b then (a :) else id) (p a) ma

---------------- Writer monad------------------
newtype WriterF w a = WriterF {runWriterF :: (a, w)}
    deriving (Functor)

type Writer f = Free (WriterF f)

runWriter :: (Monoid w) => Writer w a -> (a, w)
runWriter (Pure a) = (a, mempty)
runWriter (Free f) =
    let (fa, w) = runWriterF f
        (fa', w') = runWriter fa
     in (fa', w <> w')

writerF :: (a, w) -> WriterF w a
writerF = WriterF
tellF :: w -> WriterF w ()
tellF w = WriterF ((), w)

writer :: (a, w) -> Writer w a
writer = liftF . writerF
tell :: w -> Writer w ()
tell = liftF . tellF

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen wa =
    let (a, w) = runWriter wa
     in writer ((a, w), w)

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass wa =
    let ((a, f), w) = runWriter wa
     in writer (a, f w)

testWriterM = do
    listen $ do
        tell "Hello"
        tell "World"
        pure 5

pp2 :: (Show a, Show w) => Writer w a -> String
pp2 = iter (\(WriterF (a, w)) -> concat ["(", a, ",", show w, ")"]) . fmap show

---------------- State monad ------------------
newtype StateF s a = StateF {runStateF :: s -> (a, s)}
    deriving (Functor)

getF :: StateF s s
getF = StateF (\s -> (s, s))
putF :: s -> StateF s ()
putF s = StateF (const ((), s))
modifyF :: (s -> s) -> StateF s ()
modifyF f = StateF (\s -> ((), f s))

type State s = Free (StateF s)
get = liftF getF
put = liftF . putF
modify = liftF . modifyF

testStateM = do
    x <- get
    put (x + 1)
    pure (x + 10)

ppStateM :: (Show a, Show s) => State s a -> s -> String
ppStateM (Pure a) s = show (a, s)
ppStateM (Free f) s =
    let (a, s') = runStateF f s
     in concat [show s, "->", show s', "\n", ppStateM a s']

toState :: Monoid w => WriterF w a -> State w a
toState (WriterF (a, w)) = do
    modify (<> w)
    pure a

-- tests
listM :: Free [] Int
listM = do
    x <- liftF [1, 2, 3]
    y <- liftF [10, 20]
    z <- liftF [100, 200]
    pure (x + y + z)

ppList :: (Show a) => Free [] a -> String
ppList = iter (\xs -> concat ["[", intercalate "," xs, "]"]) . fmap show

-- test2 :: Num a => Free [] a
test2 :: Free [] [Integer]
test2 = filterM (const $ liftF [True, False]) [1 .. 3]

main = do
    -- putStrLn $ ppStateM (foldFree toState testWriterM) "Init "
    -- putStrLn $ pp2 testWriterM
    -- print $ runWriter testWriterM
    print $ filterM' (\x -> [True, False]) [1, 2, 3]

-- print $ ppList $ test2
