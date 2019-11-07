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
    compare p1 p2 | l1 == l2 = EQ
                  | l1 > l2 = GT
                  | l1 < l2 = LT 
                      where (l1, l2) = (len p1, len p2)


-- zip, zipWith & unzip 
-- Task 1: zipWithIndex - List of Tuples of Index and Element
zipWithIndex :: [a] -> [(Int, a)]
--zipWithIndex l = zip [0..n-1] l where n = length l
zipWithIndex = zip [0..]

-- Task 2: Sum of Element and Index
zipAndSum :: [Int] -> [Int]
--zipAndSum l = zipWith (+) [0..n-1] l where n = length l
zipAndSum = zipWith (+) [0..]

-- Task 3: List with Zeros on Even Indexes
evenZeros :: [Int] -> [Int]
evenZeros l = map (\(x, y) -> if even x then y else 0) (zipWithIndex l)

-- Task 4: Remove Elements with Even Indexes
removeEven :: [a] -> [a]
--removeEven l = snd (unzip (filter (\(x, y) -> even x) (zipWithIndex l)))
removeEven = snd . unzip . filter (\(x, _) -> even x) . zipWithIndex

-- Task 5: List of Digit Tuples from Numbers 10..99
digits :: [Int] -> ([Int], [Int])
digits = unzip . map (`divMod` 10)
                            
