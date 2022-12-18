{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Cat.Misc where

import Data.Kind (Constraint)
import Data.Semigroup

{----------------------------------------------------
            Type abbreviations
----------------------------------------------------}
infixr 8 :^
infixl 7 :*
infixl 6 :+
infixr 1 :=>

type s :^ n = n -> s
type (:*) = (,)
type (:+) = Either
type (:=>) = (->)

{----------------------------------------------------
            Other
----------------------------------------------------}
type Unop a = a -> a
type Binop a = a -> Unop a
type Ternop a = a -> Binop a

infixl 1 <~
infixr 1 ~>

-- | Add pre and post-processing
(~>) :: forall a b a' b'. (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(f ~> h) g = h . g . f

-- | Add post- and pre-processing
(<~) :: forall a b a' b'. (b -> b') -> (a' -> a) -> ((a -> b) -> (a' -> b'))
(h <~ f) g = h . g . f

class Yes0
instance Yes0

class Yes1 a
instance Yes1 a

class Yes2 a b
instance Yes2 a b

{----------------------------------------------------
            Constraint shorthands
----------------------------------------------------}

{----------------------------------------------------
            Constraint shorthands
----------------------------------------------------}
type C1 (con :: u -> Constraint) a = con a
type C2 (con :: u -> Constraint) a b = (con a, con b)
type C3 (con :: u -> Constraint) a b c = (con a, con b, con c)
type C4 (con :: u -> Constraint) a b c d = (con a, con b, con c, con d)
type C5 (con :: u -> Constraint) a b c d e = (con a, con b, con c, con d, con e)
type C6 (con :: u -> Constraint) a b c d e f = (con a, con b, con c, con d, con e, con f)
