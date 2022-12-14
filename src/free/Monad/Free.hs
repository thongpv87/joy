{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Monad.Free where

import Data.Functor.Identity (Identity (..))

class MonadFree f m | m -> f where
  wrap :: f (m a) -> m a

instance Functor f => MonadFree f (Free f) where
  wrap = undefined

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free f) = Free (fmap g <$> f)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure fab <*> fa = fmap fab fa
  Free fab <*> fa = Free (fmap (<*> fa) fab)

instance Functor f => Monad (Free f) where
  Pure a >>= k = k a
  Free fa >>= k = Free (fmap (>>= k) fa)

liftF :: (Functor f, MonadFree f m) => f ~> Free f
liftF f = Free (Pure <$> f)

-- | Natural transformation from @Free m ~> m@
monad :: Monad m => Free m ~> m
monad (Pure a) = pure a
monad (Free ma) = do
  ma' <- ma
  monad ma'

-- | @f ~> g@ is a natural transformation from @f@ to @g@. A natrual transformation @alpha@ need to obey this law:
--
-- [Naturality condition] @alpha . 'fmap' = 'fmap' . alpha@
type (~>) f g = forall x. f x -> g x

-- | map a natrual transformation @f~>g@ to a natural transformation between @Free f ~> Free g@.
-- We can see that @Free@ is functor in category of functors, in which object are funtors, and
-- morphism are natual transformation.
--TODO: relax contraint of the function, maybe we don't need @Functor f@
hoistFree :: (Functor f, Functor g) => (f ~> g) -> Free f ~> Free g
hoistFree _ (Pure a) = Pure a
hoistFree fg (Free f) = Free (fg (hoistFree fg <$> f))

-- | Interpret a free monad: producing a `monad morphism` from @Free f@ to ANY monad from a `natural transformmation` between functor
-- Monad morphism is natural transformation *plus* other laws
foldFree :: (Functor f, Monad m) => (f ~> m) -> Free f ~> m
foldFree fm = monad . hoistFree fm

-- | Tear down a 'Free' 'Monad' using iteration.
iter :: Functor f => (forall x. f x -> x) -> Free f a -> a
iter f fm = runIdentity $ foldFree (fmap Identity f) fm
