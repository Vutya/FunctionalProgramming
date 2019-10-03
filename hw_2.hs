-- Task 1: list of positive divisors
posDivisors :: Int -> [Int]
--posDivisors n = filter p [1..n] where 
--        p x = (mod n x) == 0
posDivisors n = filter (\x -> mod n x == 0) [1..n]

-- Task 2: check if the number is prime
checkPrime :: Int -> Bool
checkPrime n = check (posDivisors n) where
        --check _ 1 = False
        --check (_:l) n = (head l) == n
        --     = n /= 1 && (head (tail l) == n)
        check [1, a] = True
        check _ = False

checkPrime1 n = (length (posDivisors n)) == 2 

checkPrime2 :: Int -> Bool
checkPrime2 n = case (posDivisors n) of
                  [1, _] -> True
                  _ -> False

-- Task 3: check if the number is perfect
checkPerfect n = (sum (posDivisors n)) == 2 * n

-- Task 4: list of perfect numbers
perfectList n = filter checkPerfect [1..n]

-- Task 5: Sieve of Eratosthenes
sieve :: Int -> [Int]
sieve n = sieveHelper [2..n] [] where 
        sieveHelper (h:l) a = sieveHelper (filter (\x -> x `mod` h /= 0) l) (h:a)
        sieveHelper [] a = reverse a

-- Task 6: Twin Primes
twins1 :: Int -> [[Int]]
twins1 n = if (n < 5 && n > 0) then [] else twinsHelper [1..(div (n-1) 6)] [[3, 5]] where
        twinsHelper :: [Int] -> [[Int]] -> [[Int]]
        twinsHelper (i:h) l = if checkPrime2 (6*i+1) && checkPrime2 (6*i-1) 
                              then twinsHelper h ([6*i-1, 6*i+1]:l)
                              else twinsHelper h l
        twinsHelper [] l = reverse l
        
twins2 :: Int -> [[Int]]
twins2 n = h (sieve n) [] where
        --h (x:y:l) r = if y - x == 2 then h l ([x, y]:r) else h (y:l) r
        h (x:t@(y:l)) r = if y - x == 2 then h l ([x, y]:r) else h t r
        h _ r = reverse r

-- Extra Task: Filter
fltr :: (a -> Bool) -> [a] -> [a] 
fltr f a = helper f a [] where 
        helper f (x:l) r = helper f l (if f x then x:r else r)
        helper _ [] r = reverse r     
