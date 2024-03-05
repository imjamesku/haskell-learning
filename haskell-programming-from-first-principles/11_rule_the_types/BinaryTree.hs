data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' b tree = case tree of
  Leaf -> Node Leaf b Leaf
  (Node left a right)
    | b == a -> Node left a right
    | b < a -> Node (insert' b left) a right
    | b > a -> Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+ 1) testTree' == mapExpected then print "yup okay!" else error "test failed"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left b right) = b : preorder left ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left b right) = inorder left ++ (b : inorder right)

postorder :: BinaryTree a -> [a]
postorder = undefined

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree reducer initialValue tree = foldNode initialValue tree
  where
    foldNode acc tree = case tree of
      Leaf -> acc
      (Node left v right) ->
        let temp1 = foldNode acc left
            temp2 = reducer v temp1
         in foldNode temp2 right
