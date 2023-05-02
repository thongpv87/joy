{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cat.Experimental where

import Cat.Category
import Cat.Prelude hiding (Maybe)
import Data.Kind (Type)
import Types.Misc

class (Category m, Category n) => Functor f m n | f m -> n, f m -> n where
  fmap :: a `m` b -> f a `n` f b

class Functor f a a => Endofunctor f a
instance Functor f a a => Endofunctor f a

-- | natrual transformation alias
type f *-> g = forall m n a. (Functor f m n, Functor g m n) => f a -> g a

newtype Identity a = Identity {getIdentity :: a}

instance (CartesianClosed m) => Functor Identity m m where
  fmap :: m a b -> m (Identity a) (Identity b)
  fmap = undefined

newtype Compose f g a = Compose {getCompose :: f (g a)}

-- instance (Functor f m n, Functor g n p) => Category (Compose f g) where
--   id = undefined
--   (.) = undefined

-- liftC :: Category c => (a -> b) -> c a b

instance (Functor g m n, Functor f n p, CartesianClosed m, CartesianClosed n) => Functor (Compose f g) m p where
  fmap :: a `m` b -> (Compose f g a) `p` (Compose f g b)
  fmap = undefined

-- (a -> b) -> (F (G a) -> F (G b)) -> (Compose f g a)
--       Expected: p (Compose f g a) (Compose f g b)
--         Actual: p (f (g a)) (f (g b))
-- a `p` b ->  c a `p` c b = fmap c
-- (() * a) `p` (() * a) <=> () -> a

-- fmap f = fmap @f ((fmap @g f))

class (Category l, Category r, Functor f r l, Functor u l r) => Adjunction f u l r | f -> r l, u -> l r where
  leftAdjoint :: forall a b. f a `l` a -> b `r` u b
  rightAdjoin :: forall a b. a `r` f a -> f b `l` b
  unit :: forall a. a `r` u (f a)
  counit :: forall a. f (u a) `l` a

-- instance Adjunction f u l r => Monad (u f) where

------------------ CT experiment ----------------------------
data Mb a = Nt | J a

data L a = Nil | Cons a (L a)

instance Functor Mb (->) (->) where
  fmap _ Nt = Nt
  fmap f (J a) = J (f a)

instance Functor L (->) (->) where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

test = Compose (J (J 'c'))

------------------------------------------------------------
data State s a = State (s -> (a, s))

data Graph a b = Graph (Ports a -> GraphM (Ports b))
type GraphM = State (Port, [Comp])
data Comp = forall a b. Comp String (Ports a) (Ports b)

type Port = Int

data Ports :: Type -> Type where
  UnitP :: Ports ()
  BoolP :: Port -> Ports Bool
  IntP :: Port -> Ports Int
  DoubleP :: Port -> Ports Double
  PairP :: Ports a -> Ports b -> Ports (a :* b)
  FunP :: Graph a b -> Ports (a :=> b)
