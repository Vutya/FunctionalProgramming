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
-- Task 1: 
                            
