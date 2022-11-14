{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Data.MemoTrie
import Data.Set
import System.Console.ANSI (clearScreen)

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Store s a = Store (s -> a) s

instance HasTrie s => Functor (Store s) where
  fmap f (Store g s) = Store (memo $ f . g) s

instance (Show a, HasTrie s) => Show (Store s a) where
  show = show . extract

instance HasTrie s => Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

data Status
  = Dead
  | Alive
  deriving (Eq)

instance Show Status where
  show Dead = "."
  show Alive = "X"

type Cell = (Integer, Integer)

iterateM :: (Monad m) => Integer -> a -> (a -> m a) -> m a
iterateM 0 a _ = pure a
iterateM n a f = do
  a' <- f a
  iterateM (n -1) a' f

type Board a = Store (Integer, Integer) a

mkBoard :: [Cell] -> Board Status
mkBoard livings = Store f (0, 0)
  where
    f :: (Integer, Integer) -> Status
    f (x, y) = if (x, y) `notMember` livingCells then Dead else Alive
    livingCells = fromList livings

-- moving focus around the board
move :: Board a -> (Integer, Integer) -> Board a
move (Store f (x, y)) (dx, dy) = Store f (x + dx, y + dy)

next :: Board Status -> Status
next board =
  case livingNeighbors of
    2 -> extract board
    3 -> Alive
    _ -> Dead
  where
    livingNeighbors = length . Prelude.filter (== Alive) $ extract . move board <$> neightbors
    neightbors = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

life = iterate (extend next)

around n (Store f (x, y)) =
  let coords = [(x', y') | y' <- [y + n, y + n - 1 .. y - n], x' <- [x - n .. x + n]]
   in fmap f coords

showAround :: (Show a) => Integer -> Board a -> String
showAround n (Store f (x, y)) = unlines (show . fmap f <$> neighborsAround)
  where
    neighborsAround = [fmap (,y') [x - n .. x + n] | y' <- [y + n, y + n -1 .. y - n]]

main = do
  forM (life board1) $ \board -> do
    putStrLn $ showAround 20 board
    threadDelay 500000
    clearScreen

board1 = mkBoard [(0, 0), (2, 2), (1, 1), (0, 1), (-1, 1), (1, -2), (2, 1), (-1, -1)]
