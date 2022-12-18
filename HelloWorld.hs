{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Char (chr)
import Data.Constraint
import Data.Foldable (Foldable (toList))
import Data.Kind
import Data.Proxy
import GHC.TypeLits (KnownSymbol, symbolVal)

data Nat = Z | S Nat
  deriving (Show)

type family Plus (m :: Nat) (n :: Nat) :: Nat where
  Plus 'Z n = n
  Plus ( 'S m) n = 'S (Plus m n)

type family Mult (m :: Nat) (n :: Nat) :: Nat where
  Mult 'Z n = 'Z
  Mult ( 'S m) n = Plus n (Mult m n)

class ToChar a where
  toChar :: a -> Char

instance (Integral a) => ToChar a where
  toChar = chr . fromIntegral

toStr :: Dict (ToChar a, Foldable f) -> f a -> String
toStr Dict fa = toChar <$> toList fa

main :: IO ()
main = do
  print $ toStr Dict [65, 66, 67]
  return ()
