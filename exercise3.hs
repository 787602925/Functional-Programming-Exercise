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

--3a
-- Function to drop multiples of a number 
dropMult :: Int -> [Int] -> [Int]
dropMult x xs = [y | y <- xs, y `mod` x /= 0]

-- Function to drop all multiples of integers from list
dropAll :: [Int] -> [Int]
dropAll (x:xs) = x : dropAll (dropMult x xs)

-- Infinite primes list
primes :: [Int]
primes = dropAll [2 ..]

--Goldbach function
goldbach :: Int -> [(Int, Int)]
goldbach n
    | n <= 2 || odd n = []
    | otherwise = [(x, y) | x <- takeWhile (<= n `div` 2) primes, y <- takeWhile (<= n) primes, x + y == n, x <= y, odd x, odd y]
    

--3b
range :: [a] -> Int -> Int -> [a]
range xs m n = [x | (i, x) <- zip [0..] xs, i >= m, i <= n]

-- exercise 4
zeros :: Rose Int
zeros = Node 0 [zeros, zeros]
takeRose :: Int -> Rose a -> Rose a
takeRose 0 (Node a _ ) = Node a []
takeRose n (Node a rs) = Node a (map (takeRose (n-1)) rs)
-- 4a
mapRose :: (a -> b) -> Rose a -> Rose b
mapRose f (Node a rs) = Node (f a) (map (mapRose f) rs)

-- Define a helper function to generate natural numbers
natsHelper :: Int -> Rose Int
natsHelper n = Node n [natsHelper (2*n), natsHelper (2*n + 1)]

-- Define the infinite binary rose tree of natural numbers
nats :: Rose Int
nats = natsHelper 1

--4b
walk :: Rose a -> (a -> [b] -> b) -> b
walk (Node val children) f = f val (map (flip walk f) children)

--4c
fromWalk :: ((a -> [Rose a] -> Rose a) -> Rose a) -> Rose a
--anonymous function is defined to apply f to children recursively
fromWalk f = f (\val children -> Node val (map (fromWalk f) children))
