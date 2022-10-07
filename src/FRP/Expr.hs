module Expr where

import Data.Functor.Foldable

class Scalar a where
  scale :: a -> (a -> a)

data ExprF a f
  = VarF
  | ScalarF a
  | NegateF f
  | SumF f f
  | ProdF f f
  | ExpF f
  deriving (Show, Eq)
