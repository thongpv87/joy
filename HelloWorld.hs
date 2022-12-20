{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Arrow
import qualified Control.Category as C
import Control.Monad
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

newtype Circuit a b = Circuit {unCircuit :: a -> (b, Circuit a b)}

instance C.Category Circuit where
  id = Circuit (\a -> (a, C.id))
  (.) = dot
   where
    (Circuit c2) `dot` (Circuit c1) = Circuit $ \a ->
      let (b, c1') = c1 a
          (c, c2') = c2 b
       in (c, c2' `dot` c1')

instance Arrow Circuit where
  arr f = Circuit $ (\b -> (f b, arr f))
  first (Circuit cir) = Circuit $ \(b, d) ->
    let (c, cir') = cir b
     in ((c, d), first cir')

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit (Circuit cir) (x : xs) =
  let (b, cir') = cir x
   in b : runCircuit cir' xs

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \a ->
  let f' = f a
      (b, acc') = f' acc
   in (b, accum acc' f)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = f a b in (b', b'))

totalCir :: Num a => Circuit a a
totalCir = accum' 0 (+)

mean1 :: (Fractional a) => Circuit a a
mean1 =
  (totalCir Control.Arrow.&&& (const 1 ^>> totalCir)) >>> arr (uncurry (/))

mean2 :: (Fractional a) => Circuit a a
mean2 = proc value -> do
  t <- totalCir -< value
  v <- totalCir -< 1
  returnA -< (t / v)

main :: IO ()
main = do
  -- print $ toStr Dict [65, 66, 67]
  print $ runCircuit totalCir [1 .. 10]
  print $ runCircuit mean1 [1 .. 10]
  print $ runCircuit mean2 [1 .. 10]
  return ()
