--определение функции
-- g::Int->Int->Int - это 1ый, 2ой аргументы и результат

f :: Int -> Int
f x = x + 1

g :: Int -> Int -> Int
g 0 x = x
g x y = x

abs1 :: Int -> Int
abs1 x = if x < 0 then -x else x

abs2 :: Int -> Int
abs2 x | x < 0 = -x
       | otherwise = x

sign1 :: Int -> Int
sign1 x | x < 0 = -1
        | x == 0 = 0
        | otherwise = -1

-- рекурсивный вызов до умножения
-- если рек. вызов выполняется последним,
-- то она оптимизируется в цикл
fact :: Integer -> Integer
fact n | n < 0 = 0
       | n > 0 = n * fact(n-1)
       | n == 0 = 1

-- f 4 -> fH 4 1 -> fH 3 1*4 -> fH 2 1*4*3 -> fH 1 1*4*3*2 -> fH 0 1*4*3*2*1 -> fH 1*4*3*2*1
-- оптимизация рекурсии в цикл от Haskell
factHelper1 :: Integer -> Integer -> Integer
factHelper1 0 f = f
factHelper1 n f = factHelper1 (n-1) (f*n)

fact1 :: Integer -> Integer
fact1 n = factHelper1 n 1


factHelper2 :: Integer -> Integer -> Integer -> Integer
factHelper2 n m f | n == m = f
factHelper2 n i f = factHelper2 n (i+1) f*(i+1)

fact2 :: Integer -> Integer
fact2 n = factHelper2 n 0 1


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)     

--[Int]
----создание списков 
---- [1, 2, 3] -> [Int]
---- [42] -> [Int]
---- [[1, 2], [1, 2, 3], []] -> [[Int]]
---- операция : - присоединение головы списка
---- 5: [1, 2, 3] -> [5, 1, 2, 3]
---- 1: [] -> [1]
---- 1:2:3[] -> 1:2:[3] -> 1:[2, 3] -> [1, 2, 3]
---- диапазон [1..10] - список от 1 до 10
---- [1, 3..10] - список 1, 3, 5, 7, 9
---- [1..] - бесконечный список
--
---- пример: сумма элементов  списка
sum1 :: [Int] ->Int
sum1 [] = 0
---- тут не хвостовая рекурсия
sum1 (x:l) = x+sum1 l


sum2Helper  :: [Int] ->Int -> Int
sum2Helper [] a = a
sum2Helper (x:l) a = sum2Helper l (x+a)

sum2 :: [Int] ->Int
sum2 l = sum2Helper l 0

-- s2 [1, 2, 3] -> s2h [1, 2, 3] 0 -> 
-- (x=1, l=[2, 3]) s2h [2, 3] 1 -> 
---- (x=2, l=[3]) s2h [3] 3 ->
---- (x=3, l=[]) s2h [3] 6 -> 6

-- операции со списками
-- :
-- + - соединение списков
-- length -  длина
-- head - 1-ый элемент
-- last - последний
-- sum, product, maximum, minimum
-- take :: Int -> [a] -> [a] - взять столько-то элементов списка
-- drop :: ... - отбросить столько-то первых элементов
-- [10, 20, 30] !! 2 -> 30


main::IO()
main = do
	putStr "1"
	putStrLn "one"
	print(fib 20)
	print(fib 0)
	print(sign1 (-4))
