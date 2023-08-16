{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.String

{-
# Idea
ops:
  - text :: String -> Doc
  - hcat :: Doc -> Doc -> Doc
  - vcat :: Doc -> Doc -> Doc
  - sep :: [Doc] -> Doc -- hcat or vcat depends on the context
  - nest :: Int -> Doc -> Doc -- indent doc

data Layout = [(Int, String)]
-}

data Doc where
  Text :: String -> Doc
  HCat :: Doc -> Doc -> Doc
  VCat :: Doc -> Doc -> Doc
  Nest :: Int -> Doc -> Doc

data Tree a
  = Nil
  | Node a (Tree a) (Tree a)
  deriving (Functor, Show)

{- homomorphism condition
  tree2Doc . tree =
-}
tree2Doc :: (Show a) => Tree a -> Doc
tree2Doc Nil = Text "Nil"
tree2Doc (Node a l r) = Nest 2 (VCat (Text (show a)) (VCat (tree2Doc l) (tree2Doc r)))

-- class Layout a where
--   empty :: Layout a
--   beside :: Layout a -> Layout a -> Layout a
--   above :: Layout a -> Layout a -> Layout a

-- instance Layout [String] where
--   empty = []
--   beside = undefined
--   above = undefined

test = do
  x <- [1 .. 5]
  y <- [True, False]
  filterM (const [True, False]) [1 .. 5]

class Map k v where
  empty :: k -> Maybe v

  insert :: (Eq k) => k -> v -> (k -> Maybe v) -> (k -> Maybe v)
  insert k v m k' =
    if k' == k then Just v else m k

  lookup :: k -> (k -> Maybe v) -> Maybe v
  lookup k m = m k

data TMap k v = TMap v (M.Map k v)

constant :: v -> TMap k v
constant v = TMap v M.empty

update :: (Ord k) => k -> v -> TMap k v -> TMap k v
update k' v' (TMap v m) = TMap v (M.insert k' v' m)

(!) :: Ord k => TMap k v -> k -> v
TMap v m ! k = fromMaybe v (M.lookup k m)
infixl 9 !

instance Functor (TMap k) where
  fmap f (TMap v m) = TMap (f v) (fmap f m)

instance Applicative (TMap k) where
  pure = constant
  (TMap u m) <*> (TMap v n) = TMap (u v) (!)

instance Semigroup v => Semigroup (TMap k v) where
  m <> n = undefined

instance Monoid v => Monoid (TMap k v) where
  mempty = constant mempty

-- pure (x + y)
main :: IO ()
main = do
  putStrLn "Hello world"
  print test
