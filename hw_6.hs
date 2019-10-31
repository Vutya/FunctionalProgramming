-- Type Maybe
-- Task 1: Safely Pop Element
safePop :: [a] -> Maybe (a, [a])
safePop [] = Nothing
safePop (h:l) = Just (h, l)

-- Task 2: Safely Get Element by Index
safeGet :: Int -> [a] -> Maybe a
safeGet 0 (x:l) = Just x
safeGet n list = case safePop list of
                  Nothing -> Nothing
                  Just (_, l) -> safeGet (n-1) l


-- Type Classes
-- Task 1: Class Reversible with Reverse Method
class Reversible a where
    rev :: a -> a

instance Reversible Int where
    rev = list2num.num2list where
        num2list 0 = []
        num2list n = (n `mod` 10): num2list (n `div` 10)
        list2num = foldl (\a x -> 10 * a + x) 0

instance Reversible [a] where
    rev = foldl (\l x -> x:l) []

-- Task 2: Reversible, Show, Eq and Ord Classes for Point2D 
data Point2D = Point2D Int Int

len :: Point2D -> Double
len (Point2D x y) = sqrt $ fromIntegral (x * x + y * y)

instance Reversible Point2D where
    rev (Point2D a b) = Point2D (rev b) (rev a)

instance Show Point2D where
    show (Point2D a b) = "(" ++ show a ++ "; " ++ show b ++ ")"

instance Eq Point2D where
    (Point2D a1 b1) == (Point2D a2 b2) = (a1 == a2) && (b1 == b2)

instance Ord Point2D where
    compare p1 p2 | (len p1) == (len p2) = EQ
                  | (len p1) > (len p2) = GT
                  | (len p1) < (len p2) = LT
                            
