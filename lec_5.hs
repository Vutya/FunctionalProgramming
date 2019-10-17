-- Параметризованные алгебраические типы
-- MyList = Empty | Cons Int MyList - список чисел Int
-- data MyList a = Empty | Cons a (MyList a) - список элементов типа a
-- a - типовая переменная
-- Cons "abc" Empty <- список строк (MyList String)
-- MyList - функция из типа делает тип
-- MyList :: * -> *
-- Тип MayBe - встроен
-- data MayBe a = Just a | Nothing - есть значение | нет значеня
-- Это способ иметь возможность говорить о том, что значения нету
-- В Java недавно появился аналогичный тип Optional <T>
-- maybeHead :: [a] -> MayBe a
-- maybeHead [] = Nothing
-- maybeHead (x:_) = Just x
-- как пользоваться:
-- f :: [Int] -> Int - это сумма первых двух элементов списка
-- Чисто учебный пример, так как нифиг не оптимальный
-- f l = case maybeHead l of
--              Nothing -> 0
--              Just a -> a + (case maybeHead (tail l) of
--                                Nothing -> 0
--                                Just b -> b
--                            )
-- 
-- + невозможно использовать несуществующее значение
-- см. монады (оператор do)
-- data Either a b = Left a | Right b <- встроено
-- Left - информация об ошибке
-- Right - результат вычисления
--
-- Классы типов
-- Функции работают с некоторыми типами:
-- (+) - любые числа может сложить, но не строки и ничего похожего на число
-- show - практически что угодно превращает в строчку
-- show 42 - "42"
-- Как описать +
-- (+) :: a -> a -> a, но только если a - это число или...
-- Point, который мы ввели сами
--
-- Сделаем свою типовую функцию rev.
-- Она "переворачивает" объекты.
-- Создадим класс типов Reversable, некоторые типы будут иметь этот класс, т.е.
-- для них можно будет вызвать Rev
-- class Reversable a where (тип a имеет класс Reversable)
--      rev :: a -> a
-- теперь напишем rev для каких-нибудь типов
-- Int будет переворачиваемым.
--instance Reversable Int where
--       rev :: Int -> Int
--       rev = list2num.reverse.num2list where
-- list2um = foldl (\a x -> 10 * a + x) 0
-- num2list 0 = []
-- num2list n = (n `mod` 10): num2list (n `div` 10)
-- or
-- num2list n = h n [] where 
--                h 0 l = l
--                h n l = h (n `div` 10) ((n `mod` 10):l)
-- :t - выведет тип в консоли
-- rev 42 не сработает, так как интерпретатор не уверен в тм, что 42 -  это Int
-- rev (42::Int) сработает
-- давайте скажем, что rev умеет переворачивать списки
-- rev [42, 15] -> [51, 24]
-- instance Reversable a = Reversable [a] where
-- [a] можно перевернуть, но только если a можно перевернуть 
-- 1) rev = map rev .reverse 
-- 2) rev l = map rev (reverse l)
-- rev (42::Int)
-- rev [42::Int, 51]
-- 
-- А как написать функцию, которая требует тип определённого класса.
-- 1) f x = rev (rev x)
-- 2) f = rev.rev
-- какой тип f: Reversable a => a -> a (условие на переменнную => тип)
--
-- Класс Num - число
-- class Num a where
-- (+) :: a -> a -> a
-- (-) :: a -> a -> a
-- (*) :: a -> a -> a
-- negate :: a -> a
-- abs :: a -> a
-- signum :: a -> a
-- fromInteger :: Integer -> a	
-- :t (+) - это (+) :: Num a => a -> a -> a
--
-- class Num a => Fractional a (числа, которые можно делить) where
-- (/) :: a -> a -> a
-- ещё несколько
-- проблема 2/3 -> нет проблем
-- (length l) / 2 -> есть проблемы, поэтому используем fromIntegral для length
