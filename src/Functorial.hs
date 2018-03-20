module Functorial where

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

visit :: (Tree a -> Tree a) -> Tree a -> Tree a
visit f (Leaf x) = f (Leaf x)
visit f (Branch a b) = Branch (visit f a) (visit f b)

incAll :: Tree Int -> Tree Int
incAll (Leaf x) = Leaf (x + 1)
incAll x = visit incAll x

ex1 = Branch (Leaf 100) (Branch (Leaf 200) (Leaf 300))
ex1' = incAll ex1

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

ex1'' = fmap (+1) ex1
