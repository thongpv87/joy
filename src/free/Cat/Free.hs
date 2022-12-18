{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cat.Free where

import Cat.NaturalTransform ((~>))
import Control.Monad (ap)
import Data.Functor.Identity (Identity (..))

-- | 'Free' is a functor categorically, mapping 'Functor's to 'Monad', and maps
-- morphism between functor (natual transformation) into morphism of monad (via `foldFree`)
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

liftF :: (Functor f) => f ~> Free f
liftF f = Free (Pure <$> f)

-- | *Unique* monad morphism from @Free m@ to any other monad @m@. It is a natual transformation *plus* these laws:
--
-- [Associative] @'monad' . 'join' = 'join' . 'fmap' 'monad' . monad@
-- [Identity] @'monad' . 'pure' = 'pure'@
-- TODO: check correctness of the comment
monad :: Monad m => Free m ~> m
monad (Pure a) = pure a
monad (Free ma) = ma >>= monad

-- | Map a natrual transformation @f~>g@ to a natural transformation between @Free f ~> Free g@.
-- We can see that @Free@ is functor in category of functors, in which object are funtors, and
-- morphism are natual transformation.
-- TODO: relax contraint of the function, maybe we don't need @Functor f@
hoistFree :: (Functor f, Functor g) => (f ~> g) -> Free f ~> Free g
hoistFree _ (Pure a) = Pure a
hoistFree fg (Free f) = Free (fg (hoistFree fg <$> f))

-- | Interpret a free monad: producing a *monad morphism* from a *natural transformmation* between @f~>m@
foldFree :: (Functor f, Monad m) => (f ~> m) -> Free f ~> m
foldFree fm = monad . hoistFree fm

-- | Tear down a 'Free' 'Monad' using iteration.
iter :: Functor f => (forall x. f x -> x) -> Free f a -> a
iter f fm = runIdentity $ foldFree (fmap Identity f) fm

------------ testings ------------------------
-- class MonadFree m where
--   retract :: Free m ~> m

-- instance MonadFree m => Functor m where
--   fmap f m = m >>= (pure . f)

-- instance MonadFree m => Applicative m where
--   pure = retract . Pure
--   (<*>) = ap

-- instance (MonadFree m) => Monad m where
--   (>>=) = undefined

-- data Mb a = NT | Js a
--   deriving (Functor)

-- cvt :: Mb a -> (a -> b) -> Mb b
-- cvt NT _ = NT
-- cv (Js x) f = Js (f x)

-- instance MonadFree Mb where
--   retract (Pure a) = Js a
--   retract (Free f) = case cvt f retract of
--     Js (Js x) -> Js x
--     _ -> NT
