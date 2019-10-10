-- foldl and foldr
-- Task 1: Count Elements
countl :: [a] -> Int
countl = foldl (\x _ -> x + 1) 0

countr :: [a] -> Int
countr = foldr (\_ x -> x + 1) 0

-- Task 2: Reverse List
reversel :: [a] -> [a]
reversel = foldll (\x y -> y:x) []

reverser :: [a] -> [a]
reverser = foldr (\x y -> y++[x]) []

-- Task 3: Operator ++
plusr :: [a] -> [a] -> [a]
--plusr a b = foldr (\x y -> x:y) b a
--plusr = flip $ foldr (\x y -> x:y)
plusr = flip $ foldr (:)
-- flip f a b = f b a

-- Task 4:Filter Function
filterl :: (a -> Bool) -> [a] -> [a] 
filterl f = foldl (\x y -> if f y then x ++ [y] else x) []

filterr :: (a -> Bool) -> [a] -> [a] 
filterr f = foldr (\x y -> if f x then x:y else y) []

-- Task 5: Map Function
mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (\x y -> x ++ [f y]) []

mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\x y -> (f x):y) []

-- Task 6: ConcatMap Function
concatMapl :: (a -> [b]) -> [a] -> [b]
concatMapl f = foldl (\x y -> x ++ (f y)) []

concatMapr :: (a -> [b]) -> [a] -> [b]
concatMapr f = foldr (\x y -> (f x) ++ y) []

-- Task 7: foldl from foldr and vice versa
-- Реализуйте foldl через foldr. И потом наоборот. (Подсказка: переворачивание списка мы делали через foldl, поэтому его можно использовать во втором случае)
foldll :: (a -> b -> a) -> a -> [b] -> a
foldll f c l = foldr (\x y -> f y x) c (reverse l)

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f c l = foldl (\x y -> f y x) c (reverse l)

-- Task 8: MinMax
-- Better use foldl for minmax because of tail recursion (both solutions are almost similar)
minmaxl :: [Int] -> (Int, Int)
--minmaxl l = foldl (\x y -> (min (fst x) y, max (snd x) y)) (head l, head l) l
minmaxl (h:t) = foldl (\(mn, mx) y -> (min mn y, max mx y)) (h, h) t


-- concatMap
-- Task 1: Repeat Elements Twice
twice :: [a] -> [a]
twice = concatMap (\x -> [x, x])

-- Task 2: Filter with concatMap
filterCm :: (a -> Bool) -> [a] -> [a]
filterCm f = concatMap (\x -> if f x then [x] else [])

-- Task 3: Map with concatMap
mapCm :: (a -> b) -> [a] -> [b]
mapCm f = concatMap (\x -> [f x])
