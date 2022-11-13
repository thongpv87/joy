{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Set

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Store s a = Store (s -> a) s
  deriving (Functor)

instance (Show a) => Show (Store s a) where
  show = show . extract

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (Store f) s

newtype Grid s a = Grid (Store s (Store s a))
  deriving (Functor)
  deriving newtype (Show)

instance (Enum s) => Comonad (Grid s) where
  extract (Grid (Store f s)) = let Store f' _ = f s in f' s
  duplicate :: Grid s a -> Grid s (Grid s a)
  duplicate (Grid (Store fa a)) = Grid $ Store f a
    where
      f = Store (Grid . Store fa)

data Status
  = Dead
  | Alive
  deriving (Eq)

instance Show Status where
  show Dead = "-"
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
u, d, l, r :: Board a -> Board a
u (Store f s) = Store (\(x, y) -> f (x, y + 1)) s
d (Store f s) = Store (\(x, y) -> f (x, y - 1)) s
l (Store f s) = Store (\(x, y) -> f (x - 1, y)) s
r (Store f s) = Store (\(x, y) -> f (x + 1, y)) s

around :: Integer -> Board a -> [[a]]
around n b@(Store f s) =
  let coords x y = [fmap (,y') [x - n .. x + n] | y' <- [y + n, y + n -1 .. y - n]]
   in (fmap . fmap) f (coords 0 0)

nb :: Board Status -> (Status, [Status])
nb board = (extract board, extract . ($board) <$> [u, d, l, r, u . l, u . r, d . l, d . r])

next :: Board Status -> Status
next board
  | livingNeighbors < 2 = Dead
  | livingNeighbors == 2 = extract board
  | livingNeighbors == 3 = Alive
  | otherwise = Dead
  where
    livingNeighbors = length . Prelude.filter (== Alive) $ extract . ($board) <$> [u, d, l, r, u . l, u . r, d . l, d . r]

nextStatus :: Board (Status, [Status]) -> Status
nextStatus board = case length . Prelude.filter (== Alive) $ neighbors of
  2 -> status
  3 -> Alive
  _ -> Dead
  where
    (status, neighbors) = extract board

main = do
  putStrLn $ unlines $ show <$> around 4 board1
  iterateM 10 board1 $ \board -> do
    putStrLn $ unlines $ show <$> around 6 board
    pure $ extend next board

board1 = mkBoard [(0, 0), (2, 2), (1, 1), (0, 1), (-1, 1), (1, -2), (2, 1), (-1, -1)]

testNewBoardNav = do
  putStrLn "Board1"
  putStrLn $ unlines (show <$> around 4 board1)
  print (extract board1)
  putStrLn "Up"
  putStrLn $ unlines (show <$> around 4 (u board1))
  print (extract $ u board1)
  putStrLn "Left"
  putStrLn $ unlines (show <$> around 4 (l board1))
  print (extract $ l board1)
  putStrLn "Down"
  putStrLn $ unlines (show <$> around 4 (d board1))
  print (extract $ d board1)
  putStrLn "Down Left"
  putStrLn $ unlines (show <$> around 4 (d . l $ board1))
  print (extract $ d . l $ board1)
  putStrLn "Down Right"
  putStrLn $ unlines (show <$> around 4 (d . r $ board1))
  print (extract $ d . r $ board1)
