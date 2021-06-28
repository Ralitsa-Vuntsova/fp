module Task1 where

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Eq,Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Eq,Show,Read)

preorderTree :: Tree a -> [a]
preorderTree EmptyTree = []
preorderTree (Node value left right) = [value] ++ preorderTree left ++ preorderTree right

inorderTree :: Tree a -> [a]
inorderTree EmptyTree = []
inorderTree (Node value left right) = inorderTree left ++ [value] ++ inorderTree right

postorderTree :: Tree a -> [a]
postorderTree EmptyTree = []
postorderTree (Node value left right) = postorderTree left ++ postorderTree right ++ [value]

values :: Strategy -> (Tree a) -> [a]
values s EmptyTree = []
values s t
  | s == Preorder  = preorderTree t
  | s == Inorder   = inorderTree t
  | s == Postorder = postorderTree t
