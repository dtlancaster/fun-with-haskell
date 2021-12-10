-- 1 HIGHER ORDER: FIRST FUNCTIONS

-- 1.1 CURRYING & UNCURRYING

-- Number One
mapPair :: (a -> b -> c) -> [(a, b)] -> [c]
mapPair func tup = map (\(a, b) -> func a b) tup

-- Number Two
mapPair' :: (a -> b -> c) -> [(b, a)] -> [c]
mapPair' func tup = map (\(b, a) -> func a b) tup

-- 1.2 ZIPWITH

-- Number One
diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (\x y -> x - y)

-- Number Two
splice :: [String] -> [String] -> [String]
splice x y = zipWith (\x y -> x ++ y ++ x) x y

-- 1.3 MAP

-- Number One
sqLens :: [String] -> [Integer]
sqLens [] = []
sqLens x = map toInteger (map (^ 2) (map length x))

-- Number Two
bang :: [String] -> [String]
bang [] = []
bang x = map (++ "!") x

-- 1.4 FILTER

-- Number One
digitsOnly :: [Integer] -> [Integer]
digitsOnly [] = []
digitsOnly x = filter (\x -> x >= 0 && x <= 9) x

-- Number Two
removeXs :: [String] -> [String]
removeXs = filter (\x -> take 1 x /= "X")

-- 2 HIGHER ORDER: USING FOLDS

-- Number One
findNum :: Integer -> [Integer] -> Bool
findNum x [] = False
findNum x (y : ys) = (x == y) || findNum x ys

findNum' :: Integer -> [Integer] -> Bool
findNum' x ys = foldr (\y -> (||) (x == y)) False ys

-- Number Two
exists :: (a -> Bool) -> [a] -> Bool
exists x [] = False
exists x (y : ys) = (x y) || exists x ys

exists' :: (a -> Bool) -> [a] -> Bool
exists' x ys = foldr ((||) . x) False ys

-- Number Three
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x : xs) = if x `elem` xs then noDups xs else x : noDups xs

noDups' :: Eq a => [a] -> [a]
noDups' = foldr (\x y -> if x `elem` y then y else x : y) []

-- Number Four
countOverflow :: Integer -> [String] -> Integer
countOverflow x [] = 0
countOverflow x (y : ys) = if fromIntegral (length y) > x then 1 + countOverflow x ys else countOverflow x (y : ys)

countOverflow' :: Integer -> [String] -> Integer
countOverflow' n = foldr (\x y -> if fromIntegral (length x) > n then 1 + y else y) 0

-- Number Five
concatList :: [[a]] -> [a]
concatList [] = []
concatList (x : xs) = x ++ concatList xs

concatList' :: [[a]] -> [a]
concatList' xs = foldr (++) [] xs

-- Number Six
bindList :: (a -> [b]) -> [a] -> [b]
bindList x [] = []
bindList x xs = concatMap x xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' n = foldr (\x y -> n x ++ y) []
