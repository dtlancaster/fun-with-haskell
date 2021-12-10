-- Part 1

data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
  deriving (Show)

-- Number One
getLeaves :: LTree a -> [a]
getLeaves (LLeaf x) = [x]
getLeaves (LNode x tree1 tree2) = getLeaves tree1 ++ getLeaves tree2

-- Number Two
countNodes :: LTree a -> Integer
countNodes (LLeaf x) = 0
countNodes (LNode x tree1 tree2) = countNodes tree1 + countNodes tree2 + 1

-- Number Three
minTree :: LTree Integer -> Integer
minTree (LLeaf x) = x
minTree (LNode x tree1 tree2) = if x < min (minTree tree1) (minTree tree2) then x else min (minTree tree1) (minTree tree2)

-- Number Four
occursInLeaves :: (a -> Bool) -> LTree a -> Bool
occursInLeaves n (LLeaf x) = n x
occursInLeaves n (LNode x tree1 tree2) = occursInLeaves n tree1 || occursInLeaves n tree2

-- Number Five
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover n (LLeaf x) = x == n
checkNoCover n (LNode x tree1 tree2) = (checkNoCover n tree1 || checkNoCover n tree2) && x /= n

-- Part 2
foldTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode y t1 t2) =
  comb
    y
    (foldTree comb base t1)
    (foldTree comb base t2)

-- Number One
getLeaves' :: LTree a -> [a]
getLeaves' = foldTree (\x tree1 tree2 -> tree1 ++ tree2) (: [])

-- Number Two
countNodes' :: LTree a -> Integer
countNodes' = foldTree (\x tree1 tree2 -> tree1 + tree2 + 1) (const 0)

-- Number Three
minTree' :: LTree Integer -> Integer
minTree' = foldTree (\x tree1 tree2 -> min x (min tree1 tree2)) id

-- Number Four
occursInLeaves' :: (a -> Bool) -> LTree a -> Bool
occursInLeaves' = foldTree (\x tree1 tree2 -> tree1 || tree2)

-- Number Five
checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' n = foldTree (\x tree1 tree2 -> n == x || (tree1 || tree2)) (n ==)
