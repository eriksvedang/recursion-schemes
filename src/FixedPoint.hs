{-# LANGUAGE DeriveFunctor #-}

module FixedPoint where

-- | A simple tree structure where the recursion points are represented by the type variable 'r'
data TreeF r = Leaf Int
             | Branch r r
             deriving (Show, Eq, Functor)

-- | The type of these becomes longer and longer...
ex1 :: TreeF r
ex1 = Leaf 1

ex2 :: TreeF (TreeF r)
ex2 = Branch (Leaf 1) (Leaf 2)

ex3 :: TreeF (TreeF (TreeF r))
ex3 = Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 30)

data Fix f = Fx (f (Fix f))

-- | A way of getting something out of 'Fx'
outF :: Fix f -> f (Fix f)
outF (Fx x) = x

-- | The fixed point of TreeF
type Tree = Fix TreeF

-- | The type of these stay the same! Need to wrap everything in the 'Fx' constructor tho.
ex4 :: Tree
ex4 = Fx (Branch (Fx $ Leaf 1) (Fx $ Leaf 2))

ex5 :: Tree
ex5 = Fx (Leaf 1000)

-- | An algebra and a catamorphism function:
type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . outF

total :: Tree -> Int
total = cata phi where
  phi (Leaf x) = x
  phi (Branch a b) = a + b
