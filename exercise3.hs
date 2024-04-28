-- exercise 1a
universalQ :: (a -> Bool) -> [a] -> Bool
universalQ f ls = foldr (&&) True (map f ls) 

-- exercise 1b
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ls -> f x : ls) [] xs

-- exercise 1c
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x ls -> x : filter (/= x) ls) []

-- exercise 2
data Rose a = Node a [Rose a] deriving Show
-- 2a
zipWithRose :: (a -> b -> c) -> Rose a -> Rose b -> Rose c
zipWithRose f (Node x xs) (Node y ys) = Node (f x y) (zipWithChildren xs ys)
    where
        zipWithChildren [] _ = []
        zipWithChildren _ [] = []
        zipWithChildren (x:xs) (y:ys) = zipWithRose f x y : zipWithChildren xs ys
-- test for 2a
r1 :: Rose Integer
r2 :: Rose Integer
r1 = Node 8 [Node 3 [Node (-56) [], Node 4 [], Node 987 []], Node 4 [Node 6 []]]
r2 = Node (-2) [Node 5 [Node 16 [], Node 7 []], Node (-9) [Node 1 [], Node 5 []]]

-- 2b
mapAndFold :: (a -> b) -> (b -> b -> b) -> Rose a -> b
mapAndFold f g (Node x xs) = foldr g (f x) (map (mapAndFold f g) xs)

