import Text.Show.Pretty (pPrint)

data Tree a
  = EmptyTree
  | Node {value :: a, left :: Tree a, right :: Tree a}
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) = case compare x a of
  EQ -> Node a left right -- update value
  LT -> Node a (treeInsert x left) right
  GT -> Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
x `treeElem` EmptyTree = False
x `treeElem` (Node a left right) = case compare x a of
  LT -> treeElem x left
  GT -> treeElem x right
  EQ -> True

main :: IO ()
main = do
  pPrint arr
  pPrint $ 3 `treeElem` final
  pPrint $ 123 `treeElem` final
  where
    arr = scanr treeInsert EmptyTree [8, 6, 4, 1, 7, 3, 5]
    final = last arr
