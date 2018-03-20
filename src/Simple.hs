module Simple where

data Tree = Leaf Int
          | Branch Tree Tree
          deriving (Show)

ex1 = Branch (Leaf 10) (Branch (Branch (Leaf 20) (Leaf 30)) (Leaf 40))

visit :: (Tree -> Tree) -> Tree -> Tree
visit f (Leaf x) = f (Leaf x)
visit f (Branch a b) = (Branch (visit f a) (visit f b))

incAll :: Tree -> Tree
incAll (Leaf x) = Leaf (x + 1)
incAll x = visit incAll x

decAll :: Tree -> Tree
decAll (Leaf x) = Leaf (x - 1)
decAll x = visit decAll x

ex1' = incAll ex1 -- Tada!
ex1'' = decAll ex1


-- Very easy to extend:

data Tree2 = Leaf2 Int
           | Branch2 Tree2 Tree2
           | BigBranch2 [Tree2] -- Adding one more constructor here...
           deriving (Show)

ex2 = Branch2 (Leaf2 10) (Branch2 (Branch2 (Leaf2 20) (Leaf2 30)) (BigBranch2 [Leaf2 40, Leaf2 50, Leaf2 60]))

visit2 :: (Tree2 -> Tree2) -> Tree2 -> Tree2
visit2 f (Leaf2 x) = f (Leaf2 x)
visit2 f (Branch2 a b) = Branch2 (visit2 f a) (visit2 f b)
visit2 f (BigBranch2 xs) = BigBranch2 (map f xs) -- ...only need to handle it here, not below!

incAll2 :: Tree2 -> Tree2
incAll2 (Leaf2 x) = Leaf2 (x + 1)
incAll2 x = visit2 incAll2 x

decAll2 :: Tree2 -> Tree2
decAll2 (Leaf2 x) = Leaf2 (x - 1)
decAll2 x = visit2 decAll2 x

ex2' = incAll2 ex2
ex2'' = decAll2 ex2
