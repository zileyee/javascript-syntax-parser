{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Assignment where

import Instances
import Parser
import Control.Applicative (Alternative (..), optional)
import Data.List (intercalate)
import Data.Char (isAlphaNum)

-- | Data Type for Arithmetic, Logic and Comparison Operators 
data ArithOperator
  = Add | Sub | Mul | Div | Pow
  deriving (Eq)

instance Show ArithOperator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "**"

data LogicOperator
  = And | Or
  deriving (Eq)

instance Show LogicOperator where
  show And = "&&"
  show Or = "||"

data CompareOperator
  = Equals | NotEquals | LessThan | GreaterThan
  deriving (Eq)

instance Show CompareOperator where
  show Equals = "==="
  show NotEquals = "!=="
  show LessThan = "<"
  show GreaterThan = ">"

-- The main ADT for parsing
-- Represents various forms of abstract syntax tree (AST)
data ADT
  -- | Literals
  = IntLiteral Int
  | StringLiteral String
  | BoolLiteral Bool
  | ListLiteral [ADT]
  | Varname String

  -- Expressions
  | Not ADT
  | LogicExpression LogicOperator ADT ADT
  | ArithExpression ArithOperator ADT ADT
  | CompareExpression CompareOperator ADT ADT
  | Ternary ADT ADT ADT

  -- Statements
  | ConstDeclaration ADT ADT
  | CodeBlock [ADT]
  | IfStatement ADT ADT (Maybe ADT)
  | FunctionCallStatement ADT [ADT]
  | FunctionCall ADT [ADT]
  | ReturnStatement ADT
  | FunctionDeclaration ADT [ADT] [ADT]
  deriving (Eq)

-- Show instance for ADT to pretty print the parsed expression/statement
instance Show ADT where
  show (IntLiteral i) = show i
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (BoolLiteral b) = if b then "true" else "false"
  show (ListLiteral items) = "[" ++ intercalate ", " (map show items) ++ "]"
  show (Varname name) = name
  show (ArithExpression op lhs rhs) = showBinary op lhs rhs
  show (LogicExpression op lhs rhs) = showBinary op lhs rhs
  show (Not expr) = "(!" ++ show expr ++ ")"
  show (CompareExpression op lhs rhs) = showBinary op lhs rhs
  show (Ternary cond trueExpr falseExpr) 
    = "(" ++ show cond ++ " ? " ++ show trueExpr 
    ++ " : " ++ show falseExpr ++ ")"
  show (ConstDeclaration varName expr) 
    = "const " ++ show varName ++ " = " ++ show expr ++ ";"
  show (CodeBlock []) = "{ }"
  show (CodeBlock [stmt]) = "{ " ++ show stmt ++ " }"
  show (CodeBlock statements) =
      "{\n" ++ intercalate "\n" (map (\stmt -> "  " 
      ++ show stmt) statements) ++ "\n}"
  show (IfStatement condition ifBlock Nothing) =
      "if ( " ++ show condition ++ " ) " ++ show ifBlock
  show (IfStatement condition ifBlock (Just elseBlock)) =
      "if ( " ++ show condition ++ " ) " ++ show ifBlock 
        ++ " else " ++ show elseBlock
  show (FunctionCallStatement name args) 
    = show name ++ "(" ++ intercalate ", " (map show args) ++ ");"
  show (FunctionCall name args) 
    = show name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ReturnStatement expr) = "return " ++ show expr ++ ";"
  show (FunctionDeclaration name params body) =
      "function " ++ show name ++ "(" ++ intercalate ", " 
      (map show params) ++ ") " ++ show (CodeBlock body)

-- Helper functions to pretty print binary expressions
showBinary :: (Show a, Show b, Show c) => a -> b -> c -> String
showBinary op lhs rhs = "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"

-- |Helper Parsers

-- Parses an expression enclosed in parentheses and the spaces around it
parens :: Parser a -> Parser a
parens p = charTok '(' *> p <* charTok ')'

-- Determines if a given ADT expression spans multiple lines
isMultiLine :: ADT -> Bool
isMultiLine expr = length (show expr) > 42

-- Parses a list of items separated by a given separator
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

-- Parses spaces followed by a comma token
spacesCommaTok :: Parser Char
spacesCommaTok = spaces *> commaTok

-- | Parsers for Exercise A

-- Parses an integer
parseInteger :: Parser ADT
parseInteger = spaces >> int >>= \num -> pure (IntLiteral num)

-- Parses a string enclosed in double quotes
parseString :: Parser ADT
parseString = do
  content <- is '\"' *> many (isNot '\"') <* is '\"'
  pure (StringLiteral content)

-- Parses true
parseTrue :: Parser ADT
parseTrue = string "true" >> pure (BoolLiteral True)

-- Parses false
parseFalse :: Parser ADT
parseFalse = string "false" >> pure (BoolLiteral False)

-- Parser combinator that parses boolean literals
parseBoolean :: Parser ADT
parseBoolean = parseTrue <|> parseFalse

-- Parses a list of items enclosed in square brackets
parseList :: Parser ADT
parseList = do
  _ <- charTok '['
  _ <- spaces
  items <- parseLiterals `sepBy` spacesCommaTok
  _ <- spaces
  _ <- charTok ']'
  return $ ListLiteral items

-- Parser combinator that parses all literals
parseLiterals :: Parser ADT
parseLiterals =
  parseFunctionCall <|>
  parseInteger <|>
  parseString <|>
  parseBoolean <|>
  parseList <|>
  parseVarName

-- Parses a not expression
parseNot :: Parser ADT
parseNot = do
  _ <- charTok '('
  _ <- charTok '!'
  expr <- parseNot <|> parseLogicExpr <|> parseLiterals
  _ <- spaces
  _ <- charTok ')'
  pure (Not expr)

-- Parses '&&' operator
parseAnd :: Parser LogicOperator
parseAnd = stringTok "&&" >> pure And

-- Parses '||' operator
parseOr :: Parser LogicOperator
parseOr = stringTok "||" >> pure Or

-- Parser combinator that parses both '&&' and '||' operators
parseLogicOperator :: Parser LogicOperator
parseLogicOperator = parseAnd <|> parseOr

-- Parses all logical expressions
parseLogicExpr :: Parser ADT
parseLogicExpr = do
  _ <- charTok '('
  lhs <- parseExpr <|> parseLogicExpr <|> parseNot <|> parseLiterals
  _ <- spaces
  op <- tok parseLogicOperator
  rhs <- parseExpr <|> parseLogicExpr <|> parseNot <|> parseLiterals
  _ <- spaces
  _ <- charTok ')'
  pure (LogicExpression op lhs rhs)

-- Parses '+' operator
parseAdd :: Parser ArithOperator
parseAdd = is '+' >> pure Add

-- Parses '-' operator
parseSub :: Parser ArithOperator
parseSub = is '-' >> pure Sub

-- Parses '*' operator
parseMul :: Parser ArithOperator
parseMul = is '*' >> pure Mul

-- Parses '/' operator
parseDiv :: Parser ArithOperator
parseDiv = is '/' >> pure Div

-- Parses '**' operator
parsePow :: Parser ArithOperator
parsePow = is '*' >> is '*' >> pure Pow

-- Parser combinator that parses all arithmetic operators
parseArithOperator :: Parser ArithOperator
parseArithOperator 
  = parsePow <|> parseAdd <|> parseSub <|> parseMul <|> parseDiv 

-- Parses all arithmetic expressions
parseArithExpr :: Parser ADT
parseArithExpr = do
  _ <- charTok '('
  lhs <- tok parseExpr
  op <- tok parseArithOperator
  rhs <- tok parseExpr
  _ <- charTok ')'
  pure (ArithExpression op lhs rhs)

-- Parses '===' operator
parseEquals :: Parser CompareOperator
parseEquals = stringTok "===" >> pure Equals

-- Parses '!==' operator
parseNotEquals :: Parser CompareOperator
parseNotEquals = stringTok "!==" >> pure NotEquals

-- Parses '<' operator
parseLessThan :: Parser CompareOperator
parseLessThan = is '<' >> pure LessThan

-- Parses '>' operator
parseGreaterThan :: Parser CompareOperator
parseGreaterThan = is '>' >> pure GreaterThan

-- Parser combinator that parses all comparison operators
parseCompareOperator :: Parser CompareOperator
parseCompareOperator 
  = parseEquals <|> parseNotEquals <|> parseLessThan <|> parseGreaterThan

-- Parses all comparison expressions
parseCompareExpr :: Parser ADT
parseCompareExpr = do
  _ <- charTok '('
  lhs <- tok (parseExpr <|> parseLiterals)
  op <- tok parseCompareOperator
  rhs <- tok (parseExpr <|> parseLiterals)
  _ <- charTok ')'
  pure (CompareExpression op lhs rhs)

-- Parses ternary expressions
parseTernaryExpr :: Parser ADT
parseTernaryExpr = do
  _ <- charTok '('
  condition <- tok parseExpr
  _ <- charTok '?'
  trueExpr <- tok parseExpr
  _ <- charTok ':'
  falseExpr <- tok parseExpr
  _ <- charTok ')'
  pure (Ternary condition trueExpr falseExpr)

-- Parses all expressions
parseExpr :: Parser ADT
parseExpr = spaces *> (parseFunctionCall <|> parseLogicExpr <|> parseArithExpr <|> 
            parseCompareExpr <|> parseTernaryExpr <|> parseLiterals) <* spaces

-- | Exercise A

parseExerciseA :: Parser ADT
parseExerciseA = parseExpr

-- Pretty print the parsed expression
prettyPrintExerciseA :: ADT -> String

-- It deals with the case where the ternary expression spans multiple lines
-- Otherwise it just utilizes the Show instance of ADT
prettyPrintExerciseA tern@(Ternary cond trueExpr falseExpr)
  | isMultiLine tern =
      "(" ++
      prettyPrintExerciseA cond ++
      "\n? " ++
      prettyPrintExerciseA trueExpr ++
      "\n: " ++
      prettyPrintExerciseA falseExpr ++
      ")"
  | otherwise =
      show tern
prettyPrintExerciseA otherExpr = show otherExpr

-- | Parsers for Exercise B

-- Parses a variable name
parseVarName :: Parser ADT
parseVarName = do
  firstChar <- alpha
  restChars <- many (satisfy (\c -> isAlphaNum c || c == '_'))
  pure (Varname (firstChar : restChars))

-- Parses a const declaration
parseConstFunc :: Parser ADT
parseConstFunc = do
  _ <- stringTok "const"
  varName <- tok parseVarName
  _ <- charTok '='
  expr <- tok (parseExpr <|> parseFunctionCall)
  _ <- charTok ';'
  pure (ConstDeclaration varName expr)

-- Parses all statements within a code block
parseStatement :: Parser ADT
parseStatement = parseReturnStatement <|> parseIfStatement <|> parseConstFunc

-- Parses a code block
codeBlock :: Parser ADT
codeBlock = do
  _ <- charTok '{'
  decls <- many parseStatement
  _ <- charTok '}'
  pure (CodeBlock decls)

-- Parses 'if' token
parseIf :: Parser String
parseIf = stringTok "if"

-- Parses 'else' token
parseElse :: Parser String
parseElse = stringTok "else"

-- Parses an if statement with an optional else block
parseIfStatement :: Parser ADT
parseIfStatement = do
  _ <- tok parseIf
  condition <- parens parseExpr
  ifBlock <- codeBlock
  -- We use optional Maybe type class to parse the optional else block
  elseBlock <- optional (parseElse *> codeBlock)
  pure (IfStatement condition ifBlock elseBlock)

-- Parses all statements needed in Exercise B
parseAllStatement :: Parser ADT
parseAllStatement = parseIfStatement <|> codeBlock <|> parseConstFunc

-- Parses multiple statements repeatedly
parseMultipleStatements :: Parser ADT
parseMultipleStatements = many (spaces *> parseAllStatement <* spaces) 
                        >>= \decls -> pure (ListLiteral decls)

-- | Exercise B

parseExerciseB :: Parser ADT
parseExerciseB = parseMultipleStatements

-- Since there might be a list of statements, 
-- we need to pretty print each of them separated by a newline
prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (ListLiteral declarations) 
  = intercalate "\n" (map prettyPrintExerciseB declarations)
prettyPrintExerciseB otherExpr = show otherExpr

-- | Exercise C

-- Parses a list of parameters consisting of variable names
parseParameter :: Parser [ADT]
parseParameter = do
  parseVarName `sepBy` spacesCommaTok

-- Parses a function call statement
parseFunctionCallStatement :: Parser ADT
parseFunctionCallStatement = do
  name <- tok parseVarName
  args <- parens (parseExpr `sepBy` spacesCommaTok)
  _ <- charTok ';'
  pure (FunctionCallStatement name args)

-- Parses a function call
parseFunctionCall :: Parser ADT
parseFunctionCall = do
  name <- tok parseVarName
  args <- parens (parseExpr `sepBy` spacesCommaTok)
  pure (FunctionCall name args)

-- Parses a return statement
parseReturnStatement :: Parser ADT
parseReturnStatement = do
  _ <- stringTok "return"
  expr <- tok parseExpr
  _ <- charTok ';'
  pure (ReturnStatement expr)

-- Parses a function declaration
parseFunctionDeclaration :: Parser ADT
parseFunctionDeclaration = do
  _ <- stringTok "function"
  fname <- tok parseVarName
  params <- parens parseParameter
  body <- codeBlock
  -- It will return an empty list if the `codeBlock` parser fails
  let fbody = case body of
                CodeBlock items -> items
                _ -> []
  pure (FunctionDeclaration fname params fbody)

-- Checks if the function is tail recursive
isTailRecursive :: String -> Bool
isTailRecursive code = case parse parseExerciseC code of
  Result _ (FunctionDeclaration _ params body) ->
      (length params == argCountOfLastReturn body) && isTailRecursiveInBody body
  Error _ -> False
  _ -> False

-- Helper function to check if the body of a function is tail recursive.
isTailRecursiveInBody :: [ADT] -> Bool
isTailRecursiveInBody [] = False
isTailRecursiveInBody [ReturnStatement (FunctionCallStatement (Varname fname) args)] =
    all validExpression args
isTailRecursiveInBody [ReturnStatement (FunctionCall (Varname fname) args)] =
    all validExpression args
isTailRecursiveInBody (_:rest) = isTailRecursiveInBody rest

-- Helper function to check if the expression is valid.
validExpression :: ADT -> Bool
validExpression (FunctionCallStatement _ _) = False
validExpression (FunctionCall _ _) = False
validExpression (Varname v) = True 
validExpression _ = True 

-- Helper function to get the number of arguments in the last return statement
argCountOfLastReturn :: [ADT] -> Int
argCountOfLastReturn [] = 0
argCountOfLastReturn [ReturnStatement (FunctionCallStatement _ args)] = length args
argCountOfLastReturn [ReturnStatement (FunctionCall _ args)] = length args
argCountOfLastReturn (_:rest) = argCountOfLastReturn rest

-- Helper function to format the parameters
formatParams :: [ADT] -> String
formatParams params = "(" ++ intercalate ", " (map extractParamName params) ++ ")"

-- Helper function to extract the parameter name
extractParamName :: ADT -> String
extractParamName (Varname name) = name
extractParamName _ = error "Expected a parameter ADT value"

-- Helper function to deconstruct the parameters
deconstructParameters :: [String] -> ADT -> String
deconstructParameters params lastStatement =
    case lastStatement of
        ReturnStatement (FunctionCall _ args) ->
            let leftHandSide = "[" ++ intercalate ", " params ++ "]"
                rightHandSide 
                  = "[" ++ intercalate ", " (map prettyPrintExerciseC args) ++ "]"
            in leftHandSide ++ " = " ++ rightHandSide ++ ";"
        _ -> error "Expected a return statement with function call"

-- | Exercise C

parseExerciseC :: Parser ADT
parseExerciseC 
  = parseFunctionDeclaration <|> parseFunctionCallStatement <|> parseConstFunc

-- It deals with the case where the function is tail recursive 
-- and a tail call optimisation is needed
prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC func@(FunctionDeclaration name params body)
    | isTailRecursive (show func) =
        "function " ++ show name ++ formatParams params ++ " {\n" ++
        "  while (true) {\n" ++
        concatMap (indent 4 . show) (init body) ++
        indent 4 (deconstructParameters (map extractParamName params) (last body))
        ++ "  }\n" ++
        "}"
    | otherwise = show func
prettyPrintExerciseC otherExpr = show otherExpr

-- Helper function for indentation
indent :: Int -> String -> String
indent n str = replicate n ' ' ++ str ++ "\n"

-- | Exercise E (Extension)

-- | Convert if-else statement to ternary expression
transformIfToTernary :: ADT -> ADT
-- Checks if the variable name is the same
transformIfToTernary stmt@(IfStatement condition 
    (CodeBlock [ConstDeclaration varName1 trueExpr]) 
    (Just (CodeBlock [ConstDeclaration varName2 falseExpr])))
    | varName1 == varName2 
    = ConstDeclaration varName1 (Ternary condition trueExpr falseExpr)
-- Converts to a return ADT
transformIfToTernary stmt@(IfStatement condition 
  (CodeBlock [ReturnStatement trueExpr]) 
  (Just (CodeBlock [ReturnStatement falseExpr]))) 
  = ReturnStatement (Ternary condition trueExpr falseExpr)
-- Return the original expression if not transformed
transformIfToTernary expr = expr 

parseExerciseE :: Parser ADT
parseExerciseE = fmap transformIfToTernary parseIfStatement

prettyPrintExerciseE :: ADT -> String
prettyPrintExerciseE = show

-- | Evaluating Expressions

-- Arithmetic evaluation
evalArith :: ADT -> Int
evalArith (IntLiteral i) = i
evalArith (ArithExpression Add lhs rhs)
  = evalArith (evalExpr lhs) + evalArith (evalExpr rhs)
evalArith (ArithExpression Sub lhs rhs) 
  = evalArith (evalExpr lhs) - evalArith (evalExpr rhs)
evalArith (ArithExpression Mul lhs rhs) 
  = evalArith (evalExpr lhs) * evalArith (evalExpr rhs)
evalArith (ArithExpression Div lhs rhs) 
  = evalArith (evalExpr lhs) `div` evalArith (evalExpr rhs)
evalArith (ArithExpression Pow lhs rhs) 
  = evalArith (evalExpr lhs) ^ evalArith (evalExpr rhs)
evalArith _ = error "Invalid arithmetic expression"

-- Logical evaluation
evalLogic :: ADT -> Bool
evalLogic (BoolLiteral b) = b
evalLogic (LogicExpression And lhs rhs) 
  = evalLogic (evalExpr lhs) && evalLogic (evalExpr rhs)
evalLogic (LogicExpression Or lhs rhs) 
  = evalLogic (evalExpr lhs) || evalLogic (evalExpr rhs)
evalLogic (Not expr) = not (evalLogic (evalExpr expr))
evalLogic _ = error "Invalid logic expression"

-- Comparison evaluation
evalCompare :: ADT -> Bool
evalCompare (CompareExpression Equals lhs rhs) 
  = evalArith (evalExpr lhs) == evalArith (evalExpr rhs)
evalCompare (CompareExpression NotEquals lhs rhs) 
  = evalArith (evalExpr lhs) /= evalArith (evalExpr rhs)
evalCompare (CompareExpression LessThan lhs rhs) 
  = evalArith (evalExpr lhs) < evalArith (evalExpr rhs)
evalCompare (CompareExpression GreaterThan lhs rhs) 
  = evalArith (evalExpr lhs) > evalArith (evalExpr rhs)
evalCompare _ = error "Invalid compare expression"

-- Main evaluator
evalExpr :: ADT -> ADT
evalExpr arith@(ArithExpression {}) = IntLiteral (evalArith arith)
evalExpr logic@(LogicExpression {}) = BoolLiteral (evalLogic logic)
evalExpr comp@(CompareExpression {}) = BoolLiteral (evalCompare comp)
evalExpr x = x  -- catch-all, returns unmodified

evaluate :: ADT -> String
evaluate (IntLiteral i) = show i
evaluate (StringLiteral s) = show s
evaluate (BoolLiteral b) = show b
evaluate (ListLiteral items) = show items
evaluate arithExpr@(ArithExpression {}) = show $ evalArith arithExpr
evaluate logExpr@(LogicExpression {}) = show $ evalLogic logExpr
evaluate compExpr@(CompareExpression {}) = show $ evalCompare compExpr
evaluate expr = "Cannot evaluate " ++ show expr

-- | Exercise F (Extension)

-- Try out the evaluator with the following command:
-- $ parse (evaluateExerciseF <$> parseExercise F) "(1 + 2)"
parseExerciseF :: Parser ADT
parseExerciseF = parseExpr

evaluateExerciseF :: ADT -> String
evaluateExerciseF = evaluate
