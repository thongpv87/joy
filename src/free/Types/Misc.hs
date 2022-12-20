{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Types.Misc where

import Data.Kind (Constraint)

{----------------------------------------------
            Common
-----------------------------------------------}
class Yes0
instance Yes0

class Yes1 a
instance Yes1 a

class Yes2 a b
instance Yes2 a b

type Unop a = a -> a
type Binop a = a -> Unop a
type Ternop a = a -> Binop a

{----------------------------------------------
             Type abbreviations
-----------------------------------------------}

infixr 8 :^
infixl 7 :*
infixl 6 :+
infixr 1 :=>

type s :^ n = n -> s
type (:*) = (,)
type (:+) = Either
type (:=>) = (->)

{----------------------------------------------
             Constraint shorthands
-----------------------------------------------}

-- type C1 (con :: u -> Constraint) a = con a
-- type C2 (con :: u -> Constraint) a b = (con a, con b)
-- type C3 (con :: u -> Constraint) a b c = (con a, con b, con c)
-- type C4 (con :: u -> Constraint) a b c d = (con a, con b, con c, con d)
-- type C5 (con :: u -> Constraint) a b c d e = (con a, con b, con c, con d, con e)
-- type C6 (con :: u -> Constraint) a b c d e f = (con a, con b, con c, con d, con e, con f)

{---------------------------------------------
             Type level computation
---------------------------------------------}
infixr 3 &&
class (a, b) => a && b
instance (a, b) => a && b

infixr 3 &+&
class (a t, b t) => (a &+& b) t
instance (a t, b t) => (a &+& b) t

class f b a => Flip f a b
instance f b a => Flip f a b

type family FoldC op b0 as where
  FoldC op z '[] = z
  FoldC op z (a : as) = a `op` FoldC op z as

type family MapC f as where
  MapC _ '[] = '[]
  MapC f (a : as) = f a : MapC f as

type AndC cs = FoldC (&&) Yes0 cs
type AllC f cs = AndC (MapC f cs)
