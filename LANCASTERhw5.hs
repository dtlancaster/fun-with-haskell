-- Dylan Lancaster Homework 5, worked with Connor Lawson
-- Problems Completed: 1.1, 1.2
-- Problems Partially Complete: 1.3, 3
-- Problems Incomplete: 2

-- Problem 1.1
data Comp a = Value a | Error String
  deriving (Show)

comp1 :: Comp Integer
comp1 = Value 4

comp2 :: Comp (Comp Integer)
comp2 = Value (Value 4)

instance Functor Comp where
  fmap func (Value x) = Value (func x)
  fmap func _ = error "This is an error message!"

unitComp :: a -> Comp a
unitComp = Value

-- if you can define a join, then bind follows generically using fmap
joinComp :: Comp (Comp a) -> Comp a
joinComp (Value (Value x)) = Value x
joinComp _ = error "This is an error message!"

-- bind function is necessary for a Monad instance
bindComp :: (a -> Comp b) -> Comp a -> Comp b
bindComp func = joinComp . fmap func

-- apply function is necessary for an Applicative instance
applyComp :: Comp (a -> b) -> Comp a -> Comp b
applyComp func xs = bindComp (`fmap` xs) func

-- Applicative should be declared first
instance Applicative Comp where
  pure = unitComp
  func <*> xs = applyComp func xs

-- Followed by the Monad declaration
instance Monad Comp where
  return = unitComp
  xs >>= x = bindComp x xs

-- Problem 1.2
data Prop a
  = Var a
  | TT
  | FF
  | And (Prop a) (Prop a)
  | Or (Prop a) (Prop a)
  | Not (Prop a)
  | Imp (Prop a) (Prop a)
  | Iff (Prop a) (Prop a)
  deriving (Show)

instance Functor Prop where
  fmap func (Var x) = Var (func x)
  fmap func TT = TT
  fmap func FF = FF
  fmap func (And x y) = And (fmap func x) (fmap func y)
  fmap func (Or x y) = Or (fmap func x) (fmap func y)
  fmap func (Not x) = Not (fmap func x)
  fmap func (Imp x y) = Imp (fmap func x) (fmap func y)
  fmap func (Iff x y) = Iff (fmap func x) (fmap func y)

unitProp :: a -> Prop a
unitProp = Var

-- if you can define a join, then bind follows generically using fmap
joinProp :: Prop (Prop a) -> Prop a
joinProp (Var x) = x
joinProp TT = TT
joinProp FF = FF
joinProp (And x y) = And (joinProp x) (joinProp y)
joinProp (Or x y) = Or (joinProp x) (joinProp y)
joinProp (Not x) = Not (joinProp x)
joinProp (Imp x y) = Imp (joinProp x) (joinProp y)
joinProp (Iff x y) = Iff (joinProp x) (joinProp y)

-- bind function is necessary for a Monad instance
bindProp :: (a -> Prop b) -> Prop a -> Prop b
bindProp func = joinProp . fmap func

-- apply function is necessary for an Applicative instance
applyProp :: Prop (a -> b) -> Prop a -> Prop b
applyProp func xs = bindProp (`fmap` xs) func

-- Applicative instance should be declared first
instance Applicative Prop where
  pure = unitProp
  func <*> xs = applyProp func xs

-- Followed by the Monad instance
instance Monad Prop where
  return = unitProp
  xs >>= x = bindProp x xs

-- Problem 1.3
data Poly a = Mono Double [a] | Sum (Poly a) (Poly a)
  deriving (Show)

poly1 = Mono 1.0 [] -- 1

poly2 = Mono 2.0 ["X", "X"] -- 2x^2

poly3 = Sum (Mono 3.0 ["Y", "Y"]) (Mono (-0.5) ["X", "Y"]) -- 3y^2-xy/2

instance Functor Poly where
  fmap func (Mono x y) = Mono x (fmap func y)
  fmap func (Sum x y) = Sum (fmap func x) (fmap func y)

unitPoly :: a -> Poly a
unitPoly x = Mono 1 [x]

-- if you can define a join, then bind follows generically using fmap
joinPoly :: Poly (Poly a) -> Poly a
joinPoly (Sum x y) = Sum (joinPoly x) (joinPoly y)

-- bind function is necessary for a Monad instance
bindPoly :: (a -> Poly b) -> Poly a -> Poly b
bindPoly func = joinPoly . fmap func

-- apply function is necessary for an Applicative instance
applyPoly :: Poly (a -> b) -> Poly a -> Poly b
applyPoly func xs = bindPoly (`fmap` xs) func

-- Applicative instance should be declared first
instance Applicative Poly where
  pure = unitPoly
  func <*> xs = applyPoly func xs

-- Followed by the Monad instance
instance Monad Poly where
  return = unitPoly
  xs >>= x = bindPoly x xs

-- Problem 2

type Vars = String -- variables

type Vals = Double -- values

type Env = [(Vars, Vals)] -- environments

env0 :: Env
env0 = [("X", 10.0), ("Y", 2.0)]

-- evalPoly :: Env -> Poly Vars -> Vals
-- evalPoly env0 poly1 = 1.0
-- evalPoly env0 poly2 = 200.0
-- evalPoly env0 poly3 = 2.0

-- Problem 3
main :: IO ()
main = do
  putStrLn "Enter a number: "
  line <- getLine
  let x = (read line :: Integer)
  -- let y = evalPoly env0 poly2      Uncommenting this line and printing y will result in the string representation of this polynomial, did not get an evaluate function working
  print poly2

-- Testing Stuff

--countOccurrences :: Eq a => [a] -> [(a,Integer)]
--countOccurrences [] = []
--countOccurrences (x:xs) = (x,1+length(filter (== x) xs)) : countOccurrences (filter (/= x) xs)

--printPoly :: Poly String -> String
--printPoly (Sum p1 p2) = printPoly p1 ++ "+" ++ printPoly p2
--printPoly (Mono c xs) = "(" ++ show c ++ ")" ++ concat ys
--        where ys = map (\(x,i) -> if i > 1 then x ++ "^" ++ show i else x) (countOccurrences xs)