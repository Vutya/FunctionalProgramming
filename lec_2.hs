factt :: Integer -> Integer
factt n = facttHelper n 1 where
        facttHelper :: Integer -> Integer -> Integer
        facttHelper 0 f = f
        facttHelper n f = facttHelper (n-1) (f*n)

factLet :: Integer -> Integer
factLet n = let
        factlHelper :: Integer -> Integer -> Integer
        factlHelper 0 f = f
        factlHelper n f = factlHelper (n-1) (f*n)
          in factlHelper n 1


-- Инфиксная/префиксная запись функций
-- func a b <- функция от двух аргументов
-- func a b (префиксная) <-> a `func` b (инфиксная)
-- если функция состоит из не-букв, то она по умолчанию инфиксная
-- conc1 [] [] или [] `conc1` []
-- ($>@) a b <-> a $>@ b
-- a + b <-> (+) a b
-- [10, 20 ,30] ++ [50]
-- (++) [10, 20, 30] [50]
-- x @ y = x + xy
-- (@) :: Int -> Int -> Int
-- x @ y = x + x * y или (@) x y = x + x * y
-- аккуратно использовать в проектах, так можно забыть, например,
-- что значит космический корабль <:]
--
-- Параметрический полиморфизм
-- При описании функций можно использовать типовые переменные
-- a - это один и тот же тип
-- length [a] -> Int
-- (++) :: [a] -> [a] -> [a]
-- push :: a -> [a] -> [a]
-- (!!) :: [a] -> Int -> [a]
--
-- Оператор case
-- расширенный if 
-- case l of
--  [] -> "empty"
--  _ -> "not empty"   
--  сопоставления с образцом как в функциях, можно guard
--  l - выражение для разбора
--
-- Функции высших порядков
-- это функции, которые принмают (или возвращают)
-- другие функции как аргументы
-- пример:
-- функция, которая применяет заданную функцию дважды
-- app2 :: (Int -> Int) -> Int -> Int
-- (Int -> Int) - функция
-- app2 f x = f (f x)
-- пример:
-- f :: Int -> Int
-- f x = x + 1
-- app2 f 5 -> 7
--
-- app2 p 5 where
--           p :: Int -> Int
--           p x = x*x  
-- Получится 625
--
-- map - функция для работы со списком
-- (см. google или Haskell Data List)
-- применяет указанную функцию ко всем элементам списка
-- примеры:
-- plus1 x = x + 1
-- map plus1 [10, 20, 30] -> [11, 21, 31]
--
-- sq x = x*x
-- map sq [1, 2, 3] -> [1, 4, 9]
-- map i ["abc" ,"xyz"] where i s = 'i':s -> ["iabc", "ixyz"]
-- i `map` ["abc", "xyz"]
-- map :: (a -> b) -> [a]
-- map _ [] = []
-- map f x = f x: map f l
--
-- ещё одна функция обработки списков
-- filter :: (a -> Bool) -> [a] -> [a]
-- список, в котором остаются только элементы  со значением true
-- filter p [10, 20, 30] where p x - x > 20 -> [30]
-- p :: Int -> Bool
--
-- Лямбда выражения
-- Функциональное программирование - это провинутые лямбда-вычисления
-- \ x -> x + 1  - функция, которая x превращает в x + 1
-- \ x y -> x + y  - сложение, 2 аргумента
--
-- map (\x -> x + 1) [10, 20] -> [11, 21]
-- map (\s -> 'i':s) ["abc", "xyz"] -> ["iabc", "ixyz"]
--     (a -> b) [a, a]
--
-- 5 % 2 = 1 - остаток от деления
-- В Haskell - это mod 5 2
--

