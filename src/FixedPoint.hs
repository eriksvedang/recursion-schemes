module FixedPoint where

data TreeF r = Leaf Int
             | Branch r r
             deriving Show

ex1 :: TreeF r
ex1 = Leaf 1

ex2 :: TreeF (TreeF r)
ex2 = Branch (Leaf 1) (Leaf 2)

ex3 :: TreeF (TreeF (TreeF r))
ex3 = Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 30)

data Fix f = Fx (f (Fix f))

outF (Fx x) = x

type Tree = Fix TreeF

ex4 :: Tree
ex4 = Fx (Branch (Fx $ Leaf 1) (Fx $ Leaf 2))



type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . outF



instance Functor TreeF where
  fmap f (Leaf x) = Leaf (f x)
