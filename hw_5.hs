-- Task 1: data Point on Plane
data Point2D = Point2D Int Int deriving Show

vectorSum :: Point2D -> Point2D -> Point2D
vectorSum (Point2D a b) (Point2D c d) = Point2D (a + c) (b + d)

vectorLength :: Point2D -> Double
vectorLength (Point2D x y) = sqrt $ fromIntegral (x * x + y * y)

-- Task 2: data Figure
data Figure = Circle Double | Rect Double Double | Triangle Double deriving Show

area :: Figure -> Double
area (Circle r) = r * r * pi
area (Rect w h) = w * h
area (Triangle a) = 0.5 * a * a * (sqrt 3.0)

-- Task 3: data MyList
data MyList = Empty | Cons Int MyList deriving Show 
-- Empty
-- Cons 42 Empty               42 `Cons` Empty
-- Cons 42 (Cons 123 Empty)    42 `Cons` (123 `Cons` Empty)
-- []
-- 42:[]
-- 42:123:[]

mySum :: MyList -> Int
mySum (Cons n l) = h 0 (Cons n l) where 
                                h c Empty = c
                                h c (Cons n l) = h (c+n) l

myLen :: MyList -> Int
myLen (Cons n l) = h 0 (Cons n l) where 
                                h c Empty = c
                                h c (Cons n l) = h (c+1) l

myRev :: MyList -> MyList
myRev list@(Cons n l) = h (Empty) list where 
                                h rl Empty = rl
                                h rl (Cons n l) = h (Cons n rl) l 
