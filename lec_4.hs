-- Tun Tuple - хранит несколько значений, мб разных типов
-- (Int, String) - это Tuple из двух значений
-- (2, "abc") - скобки и запятая внутри создают значение Tuple
f :: (Int, Int) -> Int
f (a, b) = a + b

divmod :: Int -> Int -> (Int, Int)
divmod n m = (n `div` m, n `mod` m)

-- Сопоставление с образцом срабатывает и в присваиваниях (в стандарте не факт)
g :: Int -> Int -> Int
g x y = a+b where (a, b) = divmod a b

-- Встроенная функция zip
-- zip :: [a] -> [b] -> [(a,b)]
-- пример zip [1, 2, 3] [True, False, False] -> [(1, True), (2, False), (3, False)]
-- unzip :: [(a, b)] -> ([a], [b])


-- АЛГЕБРАИЧЕСКИЕ ТИПЫ ДАННЫХ
-- ПРИМЕРЫ
-- data Bool = True | False - имя типа и конструкторы типа
data Sex = Male | Female | Other -- как enum в Java, только нечто большее - можно хранить дополнительные данные
-- data Figure = Rect Int Int | Circle Int - конструкторы и их типы аргументов
-- создать значение: Rect 2 3 (Rect - функция Rect :: Int -> Int -> Figure)

-- Площадь фигуры
-- area :: Figure -> Float
-- area (Rect w h) = w * h
-- area (Circle r) = r * r * pi
-- Используем area f where f = Rect 2 3 или area (Rect 2 3)

data Bill = Bill [(String, Int)] deriving Show -- счёт, тип и его конструктор, "заклинание в конце типа"
-- Bill [("Телефон", 10000), ("Мороженое", 2000)]
total :: Bill -> Int
total (Bill l) = sum(snd(unzip(l)))
-- 12000
