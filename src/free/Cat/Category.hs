{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}

module Cat.Category where

import Cat.Prelude
import qualified GHC.Base as B (id, (.))
import Types.Misc

{----------------------------------------------------

-----------------------------------------------------}
{----------------------------------------------------

-----------------------------------------------------}

-- | A class for categories. Instances should satisfy the laws
--
-- *Right identity* @f '.' 'id'  =  f@
-- *Left identity*  @'id' '.' f  =  f@
-- *Associativity*  @f '.' (g '.' h)  =  (f '.' g) '.' h@
class Category k where
  id :: a `k` a
  infixr 9 .
  (.) :: b `k` c -> a `k` b -> a `k` c

infixr 1 <~, ~>

-- | Add post and pre-processing
(<~) :: (Category k) => (b `k` b') -> (a' `k` a) -> ((a `k` b) -> (a' `k` b'))
(g <~ f) h = g . h . f

-- | Add pre- and post-processing
(~>) :: (Category k) => (a' `k` a) -> (b `k` b') -> ((a `k` b) -> (a' `k` b'))
f ~> h = h <~ f

-- | RULES:
-- exl (f &&& g) = f
-- exr (f &&& g) = g
class Category k => Cartesian k where
  (&&&) :: (a `k` b) -> (a `k` c) -> (a `k` (b :* c))
  exl :: (a :* b) `k` a
  exr :: (a :* b) `k` b

(***) :: (Cartesian k) => a `k` c -> b `k` d -> (a :* b) `k` (c :* d)
f *** g = (f . exl) &&& (g . exr)

first :: Cartesian k => a `k` c -> (a :* d) `k` (c :* d)
first f = f *** id

second :: Cartesian k => b `k` d -> (c :* b) `k` (c :* d)
second g = id *** g

class Category k => Cocartesian k where
  inl :: a `k` (a :+ b)
  inr :: b `k` (a :+ b)
  (|||) :: (a `k` c) -> (b `k` c) -> ((a :+ b) `k` c)

(+++) :: (Cocartesian k) => a `k` c -> b `k` d -> (a :+ b) `k` (c :+ d)
f +++ g = (inl . f) ||| (inr . g)

left :: Cocartesian k => a `k` c -> (a :+ d) `k` (c :+ d)
left f = f +++ id

right :: Cocartesian k => b `k` d -> (c :+ b) `k` (c :+ d)
right g = id +++ g

-- | Properties: there is exactly 1 arrow from One object to any object in k
class Category k => Terminal k where
  type One k
  it :: a `k` One k

class (Cartesian k) => CartesianClosed k where
  apply :: forall a b. ((a :=> b) :* a) `k` b
  apply = uncurry id
  curry :: forall a b c. ((a :* b)) `k` c -> a `k` (b :=> c)
  uncurry :: forall a b c. a `k` (b :=> c) -> (a :* b) `k` c
  uncurry f = apply . first f

{----------------------------------------------------------
            Category of function
-----------------------------------------------------------}
instance Category (->) where
  id = B.id
  (.) = (B..)

instance Cartesian (->) where
  (&&&) :: (a -> b) -> (a -> c) -> (a -> (b :* c))
  f &&& g = \a -> (f a, g a)
  exl (a, _) = a
  exr (_, b) = b

instance Cocartesian (->) where
  inl :: a -> (a :+ b)
  inl = Left
  inr :: b -> (a :+ b)
  inr = Right
  (|||) :: (a -> c) -> (b -> c) -> (a :+ b) -> c
  (f ||| _) (Left a) = f a
  (_ ||| g) (Right a) = g a

instance Terminal (->) where
  type One (->) = ()
  it _ = ()

instance CartesianClosed (->) where
  curry :: ((a :* b) -> c) -> a -> (b :=> c)
  curry f a b = f (a, b)
  uncurry :: (a -> (b :=> c)) -> (a :* b) -> c
  uncurry f (a, b) = f a b
