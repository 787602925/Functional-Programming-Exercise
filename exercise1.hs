-- 3a
-- digtSum 5
-- digtSum 140
digitSum :: Int -> Int
digitSum n | n < 10    = n
           | otherwise = digitSum (n `div` 10) + (n `mod` 10)

-- 3b
-- subset [0,-1,0] [9,0,3,-1]
-- subset [4,3,7] [5,3,4]
-- Check if an element is present in a list
myElem :: Int -> [Int] -> Bool
myElem x [] = False
myElem n (x:xs)
  | n == x = True
  | otherwise = myElem n xs
-- Check if every element of the first list is also present in the second list
subset :: [Int] -> [Int] -> Bool
subset [] _ = True
subset (x:xs) ys = myElem x ys && subset xs ys

-- 3c
-- sortedIntersect [1,3,4] [1,4,5]
sortedIntersect :: [Int] -> [Int] -> [Int]
sortedIntersect [] [] = []
sortedIntersect xs [] = []
sortedIntersect [] ys = []
sortedIntersect (x:xs) (y:ys)
  | x == y = x : sortedIntersect xs ys
  | x < y = sortedIntersect xs (y:ys)
  | otherwise = sortedIntersect (x:xs) ys

-- 3d
-- cartesian [1,2] [3,4]
cartesianHelper :: Int -> [Int] -> [(Int, Int)]
cartesianHelper _ [] = []
cartesianHelper x (y:ys) = (x, y) : cartesianHelper x ys
cartesian :: [Int] -> [Int] -> [(Int, Int)]
cartesian [] _ = []
cartesian (x:xs) ys = cartesianHelper x ys ++ cartesian xs ys

-- 4
-- "a" >--> "b" <--< "c" 
(<--<) :: String -> String -> String
s1 <--< s2
  | s1 < s2 = s1 ++ "1"
  | otherwise = s2 ++ "2"
(>-->) :: String -> String -> String
s1 >--> s2
  | s1 > s2 = s1 ++ "1"
  | otherwise = s2 ++ "2"
infixr 8 >-->
infixr 9 <--<
