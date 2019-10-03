-- Task 1: Count Elements
countl :: [a] -> Int
countl l = foldl (\x _ -> x + 1) 0 l

countr :: [a] -> Int
countr l = foldr (\_ x -> x + 1) 0 l

-- Task 2: Reverse List

