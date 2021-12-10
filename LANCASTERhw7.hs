import Data.Char -- USED FOR ISDIGIT FUNCTION

-- Name: Dylan Lancaster
-- Worked With: Jeremy Roberts

type Vars = String

data AExpr
  = Var Vars
  | Const Integer
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  deriving (Show)

data BExpr
  = TT
  | FF -- the true and false constants
  | And BExpr BExpr
  | Or BExpr BExpr
  | Not BExpr -- boolean operations
  | Eql AExpr AExpr -- equality of arithmetic expressions
  | Lt AExpr AExpr -- true if the first is less than the second
  | Lte AExpr AExpr
  deriving (Show)

data Instr
  = Assign Vars AExpr -- assign X to the value of an expression
  | IfThenElse BExpr Instr Instr -- conditional
  | While BExpr Instr -- looping construct
  | Do [Instr] -- a block of several instructions
  | Nop -- the ”do nothing” instruction
  deriving (Show)

type Program = [Instr]

type Env = [(Vars, Integer)]

update :: (Vars, Integer) -> Env -> Env
update (x, y) [] = [(x, y)]
update (x, y) ((z, z2) : envs)
  | x == z = (x, y) : envs
  | otherwise = (z, z2) : update (x, y) envs

-- Problem One

evala :: Env -> AExpr -> Integer
evala env (Var x) = case lookup x env of
  Just y -> y
  Nothing -> error $ "Error for value: " ++ x
evala env (Const x) = x
evala env (Add x y) = evala env x + evala env y
evala env (Sub x y) = evala env x - evala env y
evala env (Mul x y) = evala env x * evala env y
evala env (Div x y) = evala env x `div` evala env y

evalb :: Env -> BExpr -> Bool
evalb env TT = True
evalb env FF = False
evalb env (And x y) = evalb env x && evalb env y
evalb env (Or x y) = evalb env x || evalb env y
evalb env (Not x) = not (evalb env x)
evalb env (Eql x y) = evala env x == evala env y
evalb env (Lt x y) = evala env x < evala env y
evalb env (Lte x y) = evala env x <= evala env y

-- Problem Two

exec :: Instr -> Env -> Env
exec (Assign x t1) y = update (x, z) y where z = evala y t1
exec (IfThenElse x t1 t2) y
  | evalb y x = exec t1 y
  | otherwise = exec t2 y
exec (Do (x : xs)) y = exec (Do xs) (exec x y)
exec (While x t1) y
  | evalb y x = exec (Do [t1, While x t1]) y
  | otherwise = y
exec (Do []) y = y
exec Nop y = y

run :: Program -> Env
run p = exec (Do p) []

sum100 :: Program -- a program to add together all the numbers up to 100
sum100 =
  [ Assign "X" (Const 0), -- initialize the sum at X=0
    Assign "C" (Const 1), -- initialize the counter at C=1
    While
      (Lt (Var "C") (Const 101)) -- while C < 101, do:
      ( Do
          [ Assign "X" (Add (Var "X") (Var "C")), -- X := X + C;
            Assign "C" (Add (Var "C") (Const 1)) -- C := C + 1
          ]
      )
  ]

sum100output = lookup "X" (run sum100)

-- Problem Three
data UOps = NotOp deriving (Show)

data BOps
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | AndOp
  | OrOp
  | EqlOp
  | LtOp
  | LteOp
  | AssignOp
  deriving (Show)

data Token
  = VSym String
  | CSym Integer
  | BSym Bool
  | UOp UOps
  | BOp BOps
  | LPar
  | RPar
  | LBra
  | RBra
  | Semi
  | Keyword String
  | Err String
  | PA AExpr
  | PB BExpr
  | PI Instr
  | Block [Instr]
  deriving (Show)

classify :: String -> Token
classify "while" = Keyword "while"
classify "if" = Keyword "if"
classify "then" = Keyword "then"
classify "else" = Keyword "else"
classify "nop" = Keyword "nop"
classify "(" = LPar
classify ")" = RPar
classify "{" = LBra
classify "}" = RBra
classify "[" = LBra
classify "]" = RBra
classify "T" = BSym True
classify "F" = BSym False
classify ";" = Semi
classify "!" = UOp NotOp
classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify "*" = BOp MulOp
classify "/" = BOp DivOp
classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp
classify "==" = BOp EqlOp
classify "<" = BOp LtOp
classify "<=" = BOp LteOp
classify ":=" = BOp AssignOp
classify x | isC x = CSym (read x)
classify x | isB x = VSym x
classify _ = Err "This is an error!"

addSpace :: String -> String
addSpace [] = []
addSpace ('\\' : '/' : xs) = " \\/ " ++ addSpace xs
addSpace ('/' : '\\' : xs) = " /\\ " ++ addSpace xs
addSpace ('+' : xs) = " + " ++ addSpace xs
addSpace ('-' : xs) = " - " ++ addSpace xs
addSpace ('*' : xs) = " * " ++ addSpace xs
addSpace ('/' : xs) = " / " ++ addSpace xs
addSpace ('(' : xs) = " ( " ++ addSpace xs
addSpace (')' : xs) = " ) " ++ addSpace xs
addSpace ('{' : xs) = " { " ++ addSpace xs
addSpace ('}' : xs) = " } " ++ addSpace xs
addSpace (';' : xs) = " ; " ++ addSpace xs
addSpace ('<' : xs) = " < " ++ addSpace xs
addSpace (':' : '=' : xs) = " := " ++ addSpace xs
addSpace ('=' : '=' : xs) = " == " ++ addSpace xs
addSpace ('T' : xs) = " T " ++ addSpace xs
addSpace ('F' : xs) = " F " ++ addSpace xs
addSpace ('i' : 'f' : xs) = " if " ++ addSpace xs
addSpace ('t' : 'h' : 'e' : 'n' : xs) = " then " ++ addSpace xs
addSpace ('e' : 'l' : 's' : 'e' : xs) = " else " ++ addSpace xs
addSpace ('w' : 'h' : 'i' : 'l' : 'e' : xs) = " while " ++ addSpace xs
addSpace ('n' : 'o' : 'p' : xs) = " nop " ++ addSpace xs
addSpace (x : xs) = x : addSpace xs

isB :: String -> Bool
isB "" = False
isB "TT" = True
isB "FF" = True
isB _ = False

isC :: String -> Bool
isC "" = False
isC (x : xs) = isDigit x && q1 xs
  where
    q1 "" = True
    q1 (y : ys) = isDigit y && q1 ys

lexer :: String -> [Token]
lexer "" = []
lexer (' ' : xs) = lexer xs
lexer ('\n' : xs) = lexer xs
lexer ('(' : xs) = LPar : lexer xs
lexer (')' : xs) = RPar : lexer xs
lexer ('{' : xs) = LBra : lexer xs
lexer ('}' : xs) = RBra : lexer xs
lexer (';' : xs) = Semi : lexer xs
lexer ('+' : xs) = BOp AddOp : lexer xs
lexer ('*' : xs) = BOp MulOp : lexer xs
lexer ('-' : xs) = BOp SubOp : lexer xs
lexer ('/' : '\\' : xs) = BOp AndOp : lexer xs
lexer ('\\' : '/' : xs) = BOp OrOp : lexer xs
lexer ('=' : '=' : xs) = BOp EqlOp : lexer xs
lexer ('<' : '=' : xs) = BOp LteOp : lexer xs
lexer ('<' : xs) = BOp LtOp : lexer xs
lexer (':' : '=' : xs) = BOp AssignOp : lexer xs
lexer ('!' : xs) = UOp NotOp : lexer xs
lexer ('/' : xs) = BOp DivOp : lexer xs
lexer ('T' : xs) = BSym True : lexer xs
lexer ('F' : xs) = BSym False : lexer xs
lexer ('w' : 'h' : 'i' : 'l' : 'e' : xs) = Keyword "while" : lexer xs
lexer ('i' : 'f' : xs) = Keyword "if" : lexer xs
lexer ('t' : 'h' : 'e' : 'n' : xs) = Keyword "then" : lexer xs
lexer ('e' : 'l' : 's' : 'e' : xs) = Keyword "else" : lexer xs
lexer ('n' : 'o' : 'p' : xs) = Keyword "nop" : lexer xs
lexer (x : xs) | isDigit x = isCSym [x] xs
  where
    isCSym i "" = [CSym (read i)]
    isCSym i (x : xs)
      | isLetter x = isCSym (i ++ [x]) xs
      | otherwise = CSym (read i) : lexer (x : xs)
lexer (x : xs) | isLower x = isVSym [x] xs
  where
    isVSym i "" = [VSym i]
    isVSym i (x : xs)
      | isLetter x || isDigit x || elem x "_'" = isVSym (i ++ [x]) xs
      | otherwise = VSym i : lexer (x : xs)

-- Problem Four

parser :: [Token] -> Instr
parser input = sr input []

sr :: [Token] -> [Token] -> Instr
sr (Err x : input) _ = error ("Error! For Variable: " ++ x)
sr [] [PI x] = x
sr input (PI y : PB x : Keyword "while" : xys) = sr input (PI (While x y) : xys)
sr input (PI z : Keyword "else" : PI y : Keyword "then" : PB x : Keyword "if" : xyzs) = sr input (PI (IfThenElse x y z) : xyzs)
sr input (Keyword "nop" : xs) = sr input (PI Nop : xs)
sr input (VSym x : xs) = sr input (PA (Var x) : xs)
sr input (CSym x : xs) = sr input (PA (Const x) : xs)
sr input (BSym x : xs)
  | x = sr input (PB TT : xs)
  | otherwise = sr input (PB FF : xs)
sr input (PA y : BOp AddOp : PA x : xys) = sr input (PA (Add x y) : xys)
sr input (PA y : BOp SubOp : PA x : xys) = sr input (PA (Sub x y) : xys)
sr input (PA y : BOp MulOp : PA x : xys) = sr input (PA (Mul x y) : xys)
sr input (PA y : BOp DivOp : PA x : xys) = sr input (PA (Div x y) : xys)
sr input (PB y : BOp AndOp : PB x : xys) = sr input (PB (And x y) : xys)
sr input (PB y : BOp OrOp : PB x : xys) = sr input (PB (Or x y) : xys)
sr input (PB x : UOp NotOp : xs) = sr input (PB (Not x) : xs)
sr input (PA y : BOp EqlOp : PA x : xys) = sr input (PB (Eql x y) : xys)
sr input (PA y : BOp LtOp : PA x : xys) = sr input (PB (Lt x y) : xys)
sr input (PA y : BOp LteOp : PA x : xys) = sr input (PB (Lte x y) : xys)
sr input (PA y : BOp AssignOp : PA (Var x) : xys) = sr input (PI (Assign x y) : xys)
sr input (RPar : PA x : LPar : xs) = sr input (PA x : xs)
sr input (RPar : PB x : LPar : xs) = sr input (PB x : xs)
sr input (RBra : PI x : xs) = sr input (Block [x] : xs)
sr input (RBra : Semi : PI x : xs) = sr input (Block [x] : xs)
sr input (Block x : Semi : PI y : xys) = sr input (Block (y : x) : xys)
sr input (Block x : LBra : xs) = sr input (PI (Do x) : xs)
sr (x : input) stack = sr input (x : stack)
sr [] stack = error (show stack)

-- Problem Five

main :: IO ()
main = do
  putStrLn "Enter your filename: "
  file <- getLine
  fileText <- readFile file
  let lexFile = lexer $ "{" ++ fileText ++ "}"
  let parsed = parser lexFile
  let executed = exec parsed []
  print executed
