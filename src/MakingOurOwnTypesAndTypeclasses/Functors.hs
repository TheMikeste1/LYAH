module MakingOurOwnTypesAndTypeclasses.Functors where
  
import MakingOurOwnTypesAndTypeclasses.RecursiveDataStructures (
   BinaryTree(..)
   )


instance Functor BinaryTree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x leftNode rightNode) = Node (f x) (fmap f leftNode) (fmap f rightNode)

