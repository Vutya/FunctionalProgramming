-- Task 1: Fast Exponentiation Algorithm
-- работает за логарифм
fea :: Int -> Int -> Int
fea x n = h x n 1 where
            h _ 0 c = c 
            h a b c = if b `mod` 2 == 0 
                      then h (a * a) (b `div` 2) c
                      else h a (b - 1) (a * c)

-- Task 2: Quick Sort
partition :: Int -> [Int] -> ([Int], [Int], [Int])
-- partition v = foldr (\a (x, y, z) -> if a < v then (a:x, y, z) 
--                                           else if a > v then (x, y, a:z) 
--                                           else (x, a:y, z)) ([], [], [])
partition v = foldl f ([], [], []) where
                                    f (x, y, z) a | a < v = (a:x, y, z)
                                                  | a > v = (x, y, a:z)
                                                  | otherwise = (x, a:y, z)

quicksort :: [Int] -> [Int]
-- quicksort l = h (partition (head l) l) where
--                  h ([], y, []) = y
--                  h (x, y, z) = (h (partition (head x) x)) ++ y ++ (h (partition (head z) z))
quicksort [] = []
--quicksort (v:l) = h (partition v (v:l)) where h (x, y, z) = quicksort x ++ y ++ quicksort z
--quicksort (v:l) = quicksort x ++ y ++ quicksort z where (x, y, z) = partition v (v:l)
quicksort list@(v:l) = quicksort x ++ y ++ quicksort z where (x, y, z) = partition v list

-- Task 3: Merge Sort
divide :: [Int] -> ([Int], [Int])
divide l = splitAt ((length l) `div` 2) l

merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge l@(a:x) r@(b:y) = if a <= b then a:merge x r else b:merge l y

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort l = merge (mergeSort left) (mergeSort right) where (left, right) = divide l
