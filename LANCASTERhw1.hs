-- Part 1

-- Problem 1: Returns minimum element in a list
minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList (x : y : xs)
  | x > y = minList (y : xs)
  | x < y = minList (x : xs)
  | x == y = minList (x : xs)

-- Problem 2: Multiplies together all elements in a list
multiplyList :: [Integer] -> Integer
multiplyList [] = 1
multiplyList (x : xs) = x * multiplyList xs

-- Problem 3: Returns True if there is an odd element in the list
existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd (x : xs)
  | x `mod` 2 == 1 = True
  | x `mod` 2 == 0 = existsOdd xs

-- Problem 4: Returns Just x if there is some input x that is odd in the list
findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x : xs) = if existsOdd (x : xs) then Just x else findOdd xs

-- Problem 5: Takes in a list of strings and removes empty strings from the list
removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x : xs) = [x | x <- (x : xs), not (null x)]

-- Problem 6: Collects all non-Nothing values in the given list and puts them into a list of type [a]
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x : xs) = case x of
  Nothing -> catMaybes xs
  Just y -> y : catMaybes xs

-- Problem 7: Takes in a list of "Either" type and returns a pair of lists. All Left values go into the first and Right go into the second.
collect :: [Either a b] -> ([a], [b])
collect [] = ([], [])
collect (x : xs) = case x of
  Left y -> (y : a, b)
    where
      (a, b) = collect xs
  Right y -> (a, y : b)
    where
      (a, b) = collect xs

-- Problem 8: Returns true if the first list is a prefix of the second, returns False otherwise.
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys)
  | x == y = isPrefix xs ys
  | otherwise = False

-- Problem 9: Returns Nothing if integer n doesn't occur in the list l, otherwise returns Just k where k is the first place in a list where n occurs.
findIndex :: Integer -> [Integer] -> Maybe Integer
findIndex n xs = findHelper n xs 0

findHelper :: Integer -> [Integer] -> Integer -> Maybe Integer
findHelper n [] c = Nothing
findHelper n (x : xs) c
  | n == x = Just c
  | otherwise = findHelper n xs (c + 1)

-- Problem 10: Creates a list of a single element repeated the given number of times.
repeatInt :: a -> Integer -> [a]
repeatInt x 0 = []
repeatInt x n = x : repeatInt x (n - 1)

-- Part 2

-- Problem 1: Tags every element of the list with its position in the list, starting at zero.
addIndex :: [a] -> [(Integer, a)]
addIndex = zip [0 ..]

-- Problem 2: Swaps coordinates in a list of pairs
swapAll :: [(a, b)] -> [(b, a)]
swapAll [] = []
swapAll ((x, y) : xys) = (y, x) : swapAll xys

-- Problem 3: Recursively inspects a list of pairs, and returns Just x if there exists some pair (x,x) with equal components in the list, returns Nothing otherwise.
findDouble :: Eq a => [(a, a)] -> Maybe a
findDouble [] = Nothing
findDouble ((x, y) : xys)
  | x == y = Just x
  | True = findDouble xys

-- Problem 4: returns True if the input is of the form Just x, and returns False otherwise
defined :: Maybe a -> Bool
defined Nothing = False
defined (Just x) = True

-- Problem 5: Skips every other entry in the list
skip :: [a] -> [a]
skip [] = []
skip [x] = [x]
skip (x : (y : xys)) = x : skip xys

-- Problem 6: Removes all even elements from the given list.
removeEvens :: [Integer] -> [Integer]
removeEvens [] = []
removeEvens (x : xs)
  | even x = removeEvens xs
  | odd x = x : removeEvens xs

-- Problem 7: Multiples all entries in the list by 2
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x : xs) = x * 2 : doubleAll xs

-- Problem 8: Takes a list of lists, and returns a single list in which all the smaller lists have been concatenated together
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x : xs) = x ++ flatten xs

-- Problem 9: Return the number of times the integer n occurs in list l
countInt :: Integer -> [Integer] -> Integer
countInt n [] = 0
countInt n l
  | n == head l = 1 + countInt n (tail l)
  | otherwise = countInt n (tail l)

-- Problem 10: Similar to the above, but generalized to work for any type which implements the equality predicate ==.
countEq :: Eq a => a -> [a] -> Integer
countEq n [] = 0
countEq n (x : xs)
  | n == x = 1 + countEq n xs
  | otherwise = countEq n xs