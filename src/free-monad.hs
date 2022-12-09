module Main where

import Control.Monad (ap, join)

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

-- foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b

foldFree :: (Monad m, Functor f) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree f (Free g) = join $ f (fmap (foldFree f) g)

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

writer = liftF . writerF
tell = liftF . tellF

testWriterM = do
    tell "Hello"
    tell "World"
    pure 5

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

main = do
    putStrLn $ ppStateM (foldFree toState testWriterM) "Init "
