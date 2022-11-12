{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Expr where

import Data.Fix (Fix (..))
import Data.Functor.Classes
import Data.Functor.Foldable
import Text.Show.Deriving

class Scalar a where
  scale :: a -> (a -> a)

data UnOp
  = Sin
  | Cos
  | Log
  | Exp
  deriving (Show, Eq)

callF :: (Floating a) => UnOp -> a -> a
callF Sin = sin
callF Cos = cos
callF Log = log
callF Exp = exp

data ExprF a f
  = VarF
  | ScalarF a
  | NegateF f
  | InvertF f
  | SumF f f
  | ProdF f f
  | CallF UnOp f
  deriving (Show, Eq, Functor)

$(deriveShow1 ''ExprF)

data Expr a
  = Var
  | Scalar a
  | Negate (Expr a)
  | Invert (Expr a)
  | Sum (Expr a) (Expr a)
  | Prod (Expr a) (Expr a)
  | Call UnOp (Expr a)
  deriving (Show, Functor)

type instance Base (Expr a) = ExprF a

instance Recursive (Expr a) where
  project Var = VarF
  project (Scalar a) = ScalarF a
  project (Negate e) = NegateF e
  project (Invert e) = InvertF e
  project (Sum a b) = SumF a b
  project (Prod a b) = ProdF a b
  project (Call op e) = CallF op e

instance Corecursive (Expr a) where
  embed VarF = Var
  embed (ScalarF a) = Scalar a
  embed (NegateF e) = Negate e
  embed (InvertF e) = Invert e
  embed (SumF a b) = Sum a b
  embed (ProdF a b) = Prod a b
  embed (CallF op e) = Call op e

scalar = Fix . ScalarF

neg = Fix . NegateF

invert = Fix . InvertF

add a b = Fix (SumF a b)

mult a b = Fix (ProdF a b)

eval :: (Floating a) => a -> Fix (ExprF a) -> a
eval x = cata $ \case
  VarF -> x
  ScalarF a -> a
  NegateF e -> negate e
  InvertF e -> recip e
  SumF e1 e2 -> e1 + e2
  ProdF e1 e2 -> e1 * e2
  CallF op e -> callF op e

simplifyExprF :: (Num a, Eq a) => Fix (ExprF a) -> Fix (ExprF a)
simplifyExprF = cata $ \case
  VarF -> Fix VarF
  ScalarF x -> scalar x
  SumF (Fix (ScalarF a)) (Fix (ScalarF b)) -> scalar (a + b)
  SumF (Fix (ScalarF 0)) b -> b
  SumF a (Fix (ScalarF 0)) -> a
  ProdF (Fix (ScalarF a)) (Fix (ScalarF b)) -> scalar (a * b)
  ProdF (Fix (ScalarF 0)) _ -> scalar 0
  ProdF _ (Fix (ScalarF 0)) -> scalar 0
  ProdF (Fix (ScalarF 1)) b -> b
  ProdF a (Fix (ScalarF 1)) -> a
  x -> Fix x

ppExprF :: (Show a, Num a, Eq a) => Fix (ExprF a) -> String
ppExprF expr = flip cata (simplifyExprF expr) $ \case
  VarF -> "x"
  ScalarF a -> show a
  NegateF e -> "-(" <> e <> ")"
  InvertF e -> "1/(" <> e <> ")"
  SumF a b -> "(" <> a <> " + " <> b <> ")"
  ProdF a b -> "(" <> a <> " * " <> b <> ")"
  CallF op e -> show op <> "(" <> e <> ")"

ad :: (Floating a) => Fix (ExprF a) -> Fix (ExprF a)
ad = para $ \case
  VarF -> scalar 1
  ScalarF _ -> scalar 0
  NegateF (_, x') -> neg x'
  SumF (_, x') (_, y') -> add x' y'
  ProdF (x, x') (y, y') -> mult x y' `add` mult y x'
  CallF Sin (_, x') -> Fix (CallF Cos x')
  CallF Cos (_, x') -> neg (Fix (CallF Sin x'))
  CallF Log (x, x') -> invert x `mult` x'
  CallF Exp (x, x') -> Fix (CallF Exp x) `mult` x'
