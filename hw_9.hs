-- Functors, Applicative Functors

-- tree = TreeNode 1 (TreeNode 2 (TreeNode 5 EmptyTree EmptyTree) (TreeNode 6 EmptyTree EmptyTree)) (TreeNode 3 (TreeNode 5 EmptyTree (TreeNode 7 (TreeNode 9 EmptyTree EmptyTree) EmptyTree)) (TreeNode 8 EmptyTree EmptyTree))
-- ltree = InnerNode (InnerNode(InnerNode (Leaf 2) (Leaf 3)) (InnerNode (Leaf 4) (Leaf 5))) (Leaf 6)
-- ftree = InnerNode (InnerNode (Leaf (*2)) (Leaf (*0))) (Leaf (+100))

data Tree a = EmptyTree | TreeNode a (Tree a) (Tree a) deriving Show
data LeafTree a = Leaf a | InnerNode (LeafTree a) (LeafTree a) deriving Show

-- Task 1: Tree and LeafTree as Functors
instance Functor Tree where
    fmap f (TreeNode a l r) = TreeNode (f a) (fmap f l) (fmap f r) 
    fmap _ EmptyTree = EmptyTree

instance Functor LeafTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (InnerNode l r) = InnerNode (fmap f l) (fmap f r)

-- Task 2: Leaf Tree as Applicative Functor
instance Applicative LeafTree  where
    pure = Leaf
    f <*> (InnerNode l r) = InnerNode (f <*> l) (f <*> r)
    (InnerNode fl fr) <*> el@(Leaf a) = InnerNode (fl <*> el) (fr <*> el)
    (Leaf f) <*> (Leaf a) = Leaf (f a)

-- Task 3: Maybe Int + Maybe Int = Maybe Int
(+@+) :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
a +@+ b = (+) <$> a <*> b

-- Task 4: Maybe [Int] + Maybe [Int] = Maybe [Int]
(+++) :: [Int] -> [Int] -> [Int]
a +++ b = (+) <$> b <*> a
 
-- Task 5: Monad Log
-- Task 6: Monad IO
