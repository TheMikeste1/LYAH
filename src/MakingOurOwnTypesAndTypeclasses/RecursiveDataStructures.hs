module MakingOurOwnTypesAndTypeclasses.RecursiveDataStructures where

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq)

singletonBinaryTree :: a -> BinaryTree a
singletonBinaryTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
treeInsert x EmptyTree = singletonBinaryTree x
treeInsert x (Node a leftNode rightNode)
   | x == a = Node x leftNode rightNode
   | x < a  = Node a (treeInsert x leftNode) rightNode
   | x > a  = Node a leftNode (treeInsert x rightNode)


treeElem :: (Ord a) => a -> BinaryTree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a leftNode rightNode)
   | x == a = True
   | x < a  = treeElem x leftNode
   | x > a  = treeElem x rightNode


listToTree :: (Ord a) => [a] -> BinaryTree a
listToTree = foldr treeInsert EmptyTree


treeToList :: BinaryTree a -> [a]
treeToList EmptyTree = []
treeToList (Node a leftNode rightNode) =
   treeToList leftNode ++ a:treeToList rightNode