-- 1a
data BinaryTree a = Leaf Bool | Node (BinaryTree a) a (BinaryTree a) deriving Show
tree1 :: BinaryTree Char
tree1_left :: BinaryTree Char
tree1_right :: BinaryTree Char
tree1_left = Node (Node (Leaf True) 'l' (Leaf False)) 'u' (Node (Leaf True) 'f' (Leaf True))
tree1_right = Node (Node (Leaf False) 'i' (Leaf False)) '2' (Leaf True)
tree1 = Node tree1_left 'g' tree1_right  
-- tree1 = Node (Node (Node (Leaf True) 'l' (Leaf False)) 'u' (Node (Leaf True) 'f' (Leaf True))) 'g' (Node (Node (Leaf False) 'i' (Leaf False)) '2' (Leaf True))

tree2 :: BinaryTree Int
tree2_left :: BinaryTree Int
tree2_right :: BinaryTree Int
tree2_left = Node (Node (Leaf True) 1 (Leaf False)) 2 (Leaf True)
tree2_right = Node (Node (Leaf False) 6 (Leaf False)) 7 (Node (Leaf True) 9 (Leaf True))
tree2 = Node tree2_left 4 tree2_right  
-- tree2 = Node (Node (Node (Leaf True) 1 (Leaf False)) 2 (Leaf True)) 4 (Node (Node (Leaf False) 6 (Leaf False)) 7 (Node (Leaf True) 9 (Leaf True)))

-- 1b
flattenTree :: BinaryTree a -> [a]
flattenTree (Leaf _) = []  -- Ignore the Booleans stored in the leaves
flattenTree (Node left x right) = flattenTree left ++ [x] ++ flattenTree right

-- 1c
pathsHelper :: BinaryTree a -> [a] -> [[a]]
pathsHelper (Leaf True) path = [path]  -- Leaf with True value, return the current path
pathsHelper (Leaf _) _ = []  -- Leaf with False value, discard this path
pathsHelper (Node left x right) path =
    let newPath = path ++ [x]
        leftPaths = pathsHelper left newPath
        rightPaths = pathsHelper right newPath
    in leftPaths ++ rightPaths

paths :: BinaryTree a -> [[a]]
paths tree = pathsHelper tree []

-- 1d
-- whether a list is sorted in ascending order
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs)
    | x <= y = isSorted (y:xs)
    | otherwise = False

-- whether a list is sorted in descending order
isReverseSorted :: (Ord a) => [a] -> Bool
isReverseSorted [] = True
isReverseSorted [_] = True
isReverseSorted (x:y:xs)
    | x >= y = isReverseSorted (y:xs)
    | otherwise = False

-- to judge whether all lists in the list of list is sorted or isReverseSorted
onlySortedlists :: (Ord a) => [[a]] -> Bool
onlySortedlists [] = True
onlySortedlists (x:xs) = (isSorted x || isReverseSorted x) && onlySortedlists(xs)

-- target function
onlySortedPaths :: (Ord a) => BinaryTree a -> Bool
onlySortedPaths tree = onlySortedlists (paths tree)


-- 2a
data Nats = Zero | Succ Nats deriving Show
instance Eq Nats where
    Zero == Zero = True
    Zero ==_  = False
    _  == Zero = False
    (Succ m) == (Succ n) = m ==  n

--2b declaration for a type class Ordered
class Eq a => Ordered a where
    lt :: a -> a -> Bool
    gt :: a -> a -> Bool
    gt x y = not (lt x y)

--2c Declare the built-in type Integer and Nats
data Nats = Zero | Succ Nats deriving Show
instance Ordered Integer where
    lt x y = x < y
    gt x y = x > y
instance Ordered Nats where
    lt Zero Zero = False
    lt Zero _     = True
    lt _ Zero     = False
    lt (Succ m) (Succ n) = lt m n
    gt x y = not (lt x y)

--2d function sortDec that sorts lists of type [a] in descending order
sortDec :: Ordered a => [a] -> [a]
sortDec [] = []
sortDec (x:xs) = sortDec [y | y <- xs, gt y x] ++ [x] ++ sortDec [y | y <- xs, lt y x] 
