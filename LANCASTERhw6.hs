-- Name: Dylan Lancaster
-- Problems Completed: 1, 2, 3, 4, 5
-- Helper Methods may be taken from previous assignments and calculator.hs from lecture
-- Worked with Connor Lawson, Reece Pollard, & James Canterbury

type Vars = String

data Prop
  = Var Vars
  | Const Bool
  | And Prop Prop
  | Or Prop Prop
  | Not Prop
  | Imp Prop Prop
  | Iff Prop Prop
  | Xor Prop Prop
  deriving (Show, Eq)

prop1 = Var "X" `And` Var "Y" -- X /\ Y

prop2 = Var "X" `Imp` Var "Y" -- X -> Y

prop3 = Not (Var "X") `Or` Var "Y" -- !X \/ Y

prop4 = Not (Var "X") `Or` Not (Var "Y") -- !X \/!Y

-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp deriving (Show, Eq)

-- the type of tokens
data Token
  = VSym Vars
  | CSym Bool
  | BOp BOps
  | NotOp
  | LPar
  | RPar
  | PB Prop -- a token to store parsed boolean expressions
  | Err String
  deriving (Show, Eq)

-- NUMBER ONE

lexParse :: String -> Token
lexParse "tt" = CSym True
lexParse "ff" = CSym False
lexParse "/\\" = BOp AndOp
lexParse "\\/" = BOp OrOp
lexParse "->" = BOp ImpOp
lexParse "<->" = BOp IffOp
lexParse "<+>" = BOp XorOp
lexParse "!" = NotOp
lexParse "(" = LPar
lexParse ")" = RPar
lexParse (x : xs) = VSym (x : xs)

lexer :: String -> [Token]
lexer x = map lexParse (words (addWhiteSpace x))

addWhiteSpace :: String -> String
addWhiteSpace "" = ""
addWhiteSpace ('!' : xs) = " ! " ++ addWhiteSpace xs
addWhiteSpace ('/' : ('\\' : xs)) = " /\\ " ++ addWhiteSpace xs
addWhiteSpace ('\\' : ('/' : xs)) = " \\/ " ++ addWhiteSpace xs
addWhiteSpace ('-' : ('>' : xs)) = " -> " ++ addWhiteSpace xs
addWhiteSpace ('<' : ('-' : ('>' : xs))) = " <-> " ++ addWhiteSpace xs
addWhiteSpace ('(' : xs) = " ( " ++ addWhiteSpace xs
addWhiteSpace (')' : xs) = " ) " ++ addWhiteSpace xs
addWhiteSpace ('<' : ('>' : xs)) = " <+> " ++ addWhiteSpace xs
addWhiteSpace (x : xs) = x : addWhiteSpace xs

-- NUMBER TWO

parser :: [Token] -> Prop
parser input = sr input []

sr :: [Token] -> [Token] -> Prop
sr (Err s : input) _ = error ("Lexical Error: " ++ s)
sr [] [PB e] = e
sr input (VSym v : rs) = sr input (PB (Var v) : rs)
sr input (CSym n : rs) = sr input (PB (Const n) : rs)
sr input (PB e1 : NotOp : rs) = sr input (PB (Not e1) : rs)
sr input (PB e1 : BOp AndOp : PB e2 : rs) = sr input (PB (And e1 e2) : rs)
sr input (PB e1 : BOp OrOp : PB e2 : rs) = sr input (PB (Or e1 e2) : rs)
sr input (PB e1 : BOp ImpOp : PB e2 : rs) = sr input (PB (Imp e1 e2) : rs)
sr input (PB e1 : BOp IffOp : PB e2 : rs) = sr input (PB (Iff e1 e2) : rs)
sr input (PB e1 : BOp XorOp : PB e2 : rs) = sr input (PB (Xor e1 e2) : rs)
sr input (RPar : PB e : LPar : rs) = sr input (PB e : rs)
sr (i : input) stack = sr input (i : stack)
sr [] stack = error (show stack)

-- NUMBERS THREE & FOUR

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr (\x -> (x :) . filter (/= x)) []

fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Or f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Imp f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Iff f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Xor f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Not f) = fv f
fv _ = []

eval :: [(Vars, Bool)] -> Prop -> Bool
eval env (Var x) = case lookup x env of
  Nothing -> error $ "No value for variable " ++ x
  Just v -> v
eval env (Const b) = b
eval env (And f1 f2) = eval env f1 && eval env f2
eval env (Or f1 f2) = eval env f1 || eval env f2
eval env (Imp f1 f2) = not (eval env f1) || eval env f2
eval env (Iff f1 f2) = if eval env f1 then eval env f2 else not (eval env f2)
eval env (Not f) = not (eval env f)
eval env (Xor f1 f2) = xor (eval env f1) (eval env f2)

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

evalAll :: Prop -> [[(Vars, Bool)]] -> Bool
evalAll = any . flip eval

genEnvs :: [Vars] -> [[(Vars, Bool)]]
genEnvs = foldr (\x y -> map ((x, True) :) y ++ map ((x, False) :) y) [[]]

sat :: Prop -> Bool
sat p = evalAll p (genEnvs (fv p))

tauto :: Prop -> Bool
tauto x = all (`eval` x) (genEnvs (fv x))

contra :: Prop -> Bool
contra x = not (any (`eval` x) (genEnvs (fv x)))

equiv :: Prop -> Prop -> Bool
equiv x y = match x y (genEnvs (fv x))

match :: Prop -> Prop -> [[(Vars, Bool)]] -> Bool
match x y [] = True
match x y (z : zs)
  | eval z x == eval z y = match x y zs
  | otherwise = False

main :: IO ()
main = do
  putStrLn "Enter a proposition:"
  propInput <- getLine
  let prop = parser (lexer propInput)
  let loop prop = do
        putStrLn "Enter next command:"
        input <- getLine
        case words input of
          ("fv" : _) -> do
            print (fv prop)
            loop prop
          ("sat" : _) -> do
            print (sat prop)
            loop prop
          ("tauto" : _) -> do
            print (tauto prop)
            loop prop
          ("contra" : _) -> do
            print (contra prop)
            loop prop
          ("equiv" : _) -> do
            putStrLn "Enter a second proposition"
            propInput2 <- getLine
            let prop2 = parser (lexer propInput2)
            print (equiv prop prop2)
            loop prop
          ("new" : _) -> do
            putStrLn "Enter new proposition"
            propInput <- getLine
            let prop = parser (lexer propInput)
            loop prop
          ("quit" : _) -> return ()
          _ -> putStrLn "Parse error!" >> loop prop
  loop prop