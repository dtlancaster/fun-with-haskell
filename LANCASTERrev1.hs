-- Problems Completed: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

-- Problem 1
radius :: Double -> Double -> Double
radius x y = if (x == 0) && (y == 0) then 0 else sqrt (x ^ 2 + y ^ 2)

-- Problem 2
radius' :: (Double, Double) -> Double
radius' (x, y) = if (fst (x, y) == 0) && (snd (x, y) == 0) then 0 else sqrt ((fst (x, y) ^ 2) + (snd (x, y) ^ 2))

-- Problem 3
sumEvens :: Integer -> Integer
sumEvens 0 = 0
sumEvens n = if odd n then sumEvens (n - 1) else n + sumEvens (n - 1)

-- Problem 4
sumEvens' :: Integer -> Integer
sumEvens' 0 = 0
sumEvens' n = if odd n then sumEvens (n - 1) else sum [x | x <- [1 .. n], even x]

-- Problem 5
collatz :: Integer -> Integer
collatz n
  | n == 0 || n == 1 = 1
  | even n = collatz (n `div` 2)
  | odd n = collatz (3 * n + 1)

-- Problem 6
checkCollatz :: [Integer]
checkCollatz = map collatz [x | x <- [0 .. 100]]

-- Problem 7
div3List :: Integer -> [Integer]
div3List n = [x | x <- [1 .. n], x `mod` 3 == 0]

-- Problem 8
init' :: [a] -> [a]
init' [x] = []
init' (x : xs) = x : init' xs

-- Problem 9
findEmpty :: [String] -> Bool
findEmpty (x : xs)
  | null x = True
  | otherwise = False

-- Problem 10
getLengths :: [String] -> [Int]
getLengths = map length