-- Trees

-- tree = TreeNode 1 (TreeNode 2 (TreeNode 5 EmptyTree EmptyTree) (TreeNode 6 EmptyTree EmptyTree)) (TreeNode 3 (TreeNode 5 EmptyTree (TreeNode 7 (TreeNode 9 EmptyTree EmptyTree) EmptyTree)) (TreeNode 8 EmptyTree EmptyTree))
-- ltree = InnerNode (InnerNode(InnerNode (Leaf 2) (Leaf 3)) (InnerNode (Leaf 4) (Leaf 5))) (Leaf 6)

data Tree a = EmptyTree | TreeNode a (Tree a) (Tree a)
data LeafTree a = Leaf a | InnerNode (LeafTree a) (LeafTree a)

-- Task 1: class Show for tree
instance Show a => Show (Tree a) where
    show EmptyTree = ""
    show (TreeNode n l r) = showLeaves (TreeNode n l r) [] where
                              space = foldr (\s a -> a ++ s ++ "   ") ""
                              showLeaves EmptyTree _ = ""
                              showLeaves (TreeNode n EmptyTree EmptyTree) chars = show n ++ "\n"
                              showLeaves (TreeNode n l EmptyTree) chars = show n ++ "\n" ++ space chars ++ 
                                                                          "└── " ++ showLeaves l (" ":chars)
                              showLeaves (TreeNode n EmptyTree r) chars = show n ++ "\n" ++ space chars ++ 
                                                                          "└── " ++ showLeaves r (" ":chars)
                              showLeaves (TreeNode n l r) chars = show n ++ "\n" ++ 
                                                                  space chars ++ "├── " ++ showLeaves l ("|":chars) ++
                                                                  space chars ++ "└── " ++ showLeaves r (" ":chars)
                                                                  
-- Task 2: Fold Trees
treeFold :: b -> (a -> b -> b -> b) -> Tree a -> b
treeFold f1 f2 (TreeNode a l r) = f2 a (treeFold f1 f2 l) (treeFold f1 f2 r)
treeFold f1 _ (EmptyTree) = f1

-- Task 3: Fold Leaf Trees
leafFold :: (a -> b) -> (b -> b -> b) -> LeafTree a -> b
leafFold f1 f2 (InnerNode l r) = f2 (leafFold f1 f2 l) (leafFold f1 f2 r)
leafFold f _ (Leaf a) = f a

-- Task 4: Leaf Tree to List
listifyLeafTree :: LeafTree a -> [a]
listifyLeafTree tree = leafFold (\x -> [x]) (\x y -> x ++ y) tree

-- Task 5: Tree to List (3 ways)
listifyTreeFirst :: Tree a -> [a]
listifyTreeFirst tree = treeFold [] (\a b c -> [a] ++ b ++ c) tree

listifyTreeLast :: Tree a -> [a]
listifyTreeLast tree = treeFold [] (\a b c -> b ++ c ++ [a]) tree

listifyTreeBetween :: Tree a -> [a]
listifyTreeBetween tree = treeFold [] (\a b c -> b ++ [a] ++ c) tree

-- Task 6: Class Foldable for Tree and Leaf Tree
instance Foldable Tree where
    foldr f c t@(TreeNode a l r) = foldr f c (listifyTreeBetween t)

instance Foldable LeafTree where
    foldr f c t@(InnerNode l r) = foldr f c (listifyLeafTree t)
