module Functorial where

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

ex1 = Branch (Leaf 100) (Branch (Leaf 200) (Leaf 300))
ex1' = fmap (+1) ex1
