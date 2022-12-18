{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}

module Cat.Category where

import Cat.Misc
import Data.Kind
import qualified GHC.Base as B (id, (.))

{----------------------------------------------------

-----------------------------------------------------}
type Ok2 k a b = C2 (Ok k) a b
type Ok3 k a b c = C3 (Ok k) a b c
type Ok4 k a b c d = C4 (Ok k) a b c d
type Ok5 k a b c d e = C5 (Ok k) a b c d e
type Ok6 k a b c d e f = C6 (Ok k) a b c d e f

{----------------------------------------------------

-----------------------------------------------------}

-- | A class for categories. Instances should satisfy the laws
--
-- [Right identity] @f '.' 'id'  =  f@
-- [Left identity]  @'id' '.' f  =  f@
-- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
class Category k where
  type Ok k :: Type -> Constraint
  type Ok k = Yes1
  id :: Ok k a => a `k` a
  infixr 9 .
  (.) :: forall a b c. Ok3 k a b c => b `k` c -> a `k` b -> a `k` c

{-# RULES
"identity/left" forall p. id . p = p
"identity/right" forall p. p . id = p
"association" forall p q r. (p . q) . r = p . (q . r)
  #-}

instance Category (->) where
  id = B.id
  (.) = (B..)

-- infixr 1 <<<, >>>
-- -- | Right to left composition
-- (<<<) :: Category cat => cat b c -> cat a b -> cat a c
-- (<<<) = (.)

-- -- | Left to right composition
-- (>>>) :: Category cat => cat a b -> cat b c -> cat a c
-- ab >>> bc = bc . ab
-- {-# INLINE (>>>) #-}
