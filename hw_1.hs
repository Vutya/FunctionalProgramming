-- Basic List Operations

-- Task 1
length1 :: [String] -> Integer
length1 l = let
             lengthHelper :: [String] -> Integer -> Integer
             lengthHelper [] n = n
             lengthHelper (_:l) n = lengthHelper l (n+1)
            in
             lengthHelper l 0  

-- Task 2
last1 :: [String] -> String
--last1 l = head (rev1 l)
last1 (x:[]) = x
last1 (_:l) = last1 l

-- Task 3
-- приписать в конец - это долго
-- даже узнать длину по времени это длина списка
-- лучше переворачивать список, а ещё круче БЕЗ
concat1 :: [String] -> [String] -> [String] 
concat1 l m = concatHelper (rev1 l) m where
        concatHelper (x:l) m = concatHelper l (x:m)
        concatHelper [] m = m

-- Task 4
push1 :: String -> [String] -> [String]
push1 s l = rev1 (s:(rev1 l))

-- Task 5
repeat1 :: Int -> Char -> String
repeat1 n c = repHelp n c "" where 
        repHelp 0 _ m = m        
        repHelp n c m = repHelp (n-1) c (c:m)

-- Task 6
get1 :: Int -> [String] -> String
get1 0 (x:_) = x
get1 n (_:l) = get1 (n-1) l

-- Task 7
rev1 :: [String] -> [String]
rev1 l = let
          revHelper :: [String] -> [String] -> [String]
          revHelper [] a = a
          revHelper (x:b) c = revHelper b (x:c)
         in revHelper l []

-- Task 8
takeHelper1 :: Int -> [String] -> [String] -> [String]
takeHelper1 0 l _ = l
takeHelper1 n l (y:m) = takeHelper1 (n-1) (l++[y]) m

take1 :: Int -> [String] -> [String]
take1 n l = takeHelper1 n [] l

take2 :: Int -> [String] -> [String]
take2 0 _ = []
take2 n (x:l) = x:(take (n-1) l)

take3 :: Int -> [String] -> [String]
take3 n l = h n l [] where
        h 0 _ a = rev1 a
        h n (x:l) a = h (n-1) l (x:a)

-- Effective Fibonacci Numbers
fibHelper :: Integer -> Integer -> Integer -> Integer
fibHelper 1 a b = a + b
fibHelper n a b = fibHelper (n-1) b (a+b)

fibOptim :: Integer -> Integer
fibOptim n = fibHelper (n-2) 1 1

-- Extra Tasks
maxHelper :: [Int] -> Int -> Int
maxHelper [] m = m
--maxHelper (x:l) m | x > m = maxHelper l x
--		  | otherwise = maxHelper l m
maxHelper (x:l) m = maxHelper l (if x > m then x else m) 

myMax :: [Int] -> Int
myMax (x:l) = maxHelper l x


minHelper :: [Int] -> Int -> Int
minHelper [] m = m
minHelper (x:l) m | x < m = minHelper l x
		  | otherwise = minHelper l m 

myMin :: [Int] -> Int
myMin (x:l) = minHelper l x

drop1 :: Int -> [String] -> [String]
drop1 0 l = l
drop1 n (x:l) = drop1 (n-1) l

-- Task **:  [String] -> [Int] -> [String] - list of strings with give indexes
getsHelper :: [String] -> [Int] -> [String] -> [String]
getsHelper strs [] res = rev1 res
getsHelper strs (x:l) res = getsHelper strs l ((get1 x strs):res) 

getStrings :: [String] -> [Int] -> [String]
getStrings m nums =  getsHelper m nums []


