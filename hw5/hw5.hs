--Ryan Schreiber
--Partner: Fedor Nikolayev

-- Necessary imports
import Control.Applicative ((<$>),liftA,liftA2)
import Data.Map
import Data.Char
import Data.Maybe
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P


--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | For Variable Expression Expression Statement -- for var in e1 to e2 do s end
               | Skip                              -- no-op

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- P.identifier lexer
              P.reserved lexer "in"
              start <- exprParser
              P.reserved lexer "to"
              finish <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (For var start finish body)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)


-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

--  Uncomment the following function for question #5 and #6

-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing

--Homework 5
--Problem 1:
instance Show Value where
  show (IntVal i) = show i
  show (BoolVal b)
    | b == True = "true"
    | b == False = "false"

instance Show Op where
  show (Plus) = "+"         
  show (Minus) = "-"        
  show (Times) = "*"        
  show (GreaterThan) = ">"  
  show (Equals) = "=="      
  show (LessThan) = "<"  

instance Show Expression where
  show (Var var) = var                          -- e.g. x
  show (Val val) = show val                      -- e.g. 2
  show (BinOp op exp1 exp2)  = show exp1 ++ " " ++ show op ++ " " ++ show exp2 -- e.g. x + 3
  show (Assignment var exp1) =  var ++ " = " ++ show exp1 -- e.g. x = 3

instance Show Statement where
  show (Expr e1) = show e1
  show (If e1 s1 s2) = "if " ++ show e1 ++ " then " ++ show s1 ++ " else " ++ show s2 ++ " end"-- if e then s1 else s2 end
  show (While e1 s1) = "while " ++ show e1 ++ " do " ++ show s1 ++ " end"   -- while e do s end
  show (For v1 e1 e2 s1) = "for " ++ v1 ++ " in " ++ show e1 ++ " to " ++ show e2 ++ " do " ++ show s1 ++ " end" --for a in 1 to 2 do 3 end
  show (Sequence s1 s2) = show s1 ++";" ++ show s2   -- s1; s2
  show (Skip) = ""                          -- no-op

--Problem 2
--takes as input an expression and a store and returns a value.
--If a variable is not found (e.g. because it is not initialized)
--throw an error with the error function.
evalE :: Expression -> Store -> (Value, Store)
evalE (BinOp o a b) s = (applyOp o a' b', s'') where
          (a', s')  = evalE a s
          (b', s'') = evalE b s'
evalE (Var x) s = case (Data.Map.lookup x s) of
            Just x -> (x,s)
            Nothing -> error "variable not found"
evalE (Val v) s = (v, s)
evalE (Assignment x e) s = (val, str) where
          (val, str') = evalE e s
          str = Data.Map.insert x val str'

--Problem 3
--that takes as input a statement and a store and
--returns a possibly modified store.
evalS :: Statement -> Store -> Store
evalS w@(While e s1) s = case (evalE e s) of
                       (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
                       (BoolVal False,s') -> s'
                       _                  -> error "Condition must be a BoolVal"
evalS Skip s             = s
evalS (Expr e) s         = snd (evalE e s)
evalS (Sequence s1 s2) s = evalS s2 st where
                           st = (evalS s1 s)
evalS (If e s1 s2) s     = case (evalE e s) of
                           (BoolVal True,str)  -> (evalS s1 str)
                           (BoolVal False,str) -> (evalS s2 str)
                           _                  -> error "Condition must be a BoolVal"
evalS (For var e1 e2 s1) s = let str = evalS s1 str' --Problem 6
                                 lst = case (evalE e2 str') of
                                         (IntVal v, str') -> v
                                         _                -> error "Second value in for loop not an Int"
                                 (frst, str') = case (evalE (Assignment var e1) s) of
                                                  (IntVal z, str') -> (z, str')
                                                  _                -> error "First value in for loop not an Int"
                                 expr = Val (IntVal (frst + 1))
                                in if lst >= frst then
                                     evalS(For var expr e2 s1) str
                                else str'
--Problem 4
evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s = do val <- (Data.Map.lookup x s) ; return (val, s)
evalE_maybe (Val v) s = Just (v, s)
evalE_maybe (Assignment x e) s = do (val, str) <- evalE_maybe e s
                                    return (val, (Data.Map.insert x val str))
                                      

evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe w@(While e s1) s =  do expr <- (evalE_maybe e s) ; case expr of
                                        (BoolVal True, str)  -> do str' <- evalS_maybe s1 str
                                                                  evalS_maybe w str'
                                        (BoolVal False, str) -> return str
                                        _                   -> Nothing    
evalS_maybe Skip s             = return s
evalS_maybe (Sequence s1 s2) s = (evalS_maybe s1 s) >>= evalS_maybe s2
evalS_maybe (Expr e) s         = (evalE_maybe e s) >>= return . snd
evalS_maybe (If e s1 s2) s     = do expr <- (evalE_maybe e s) ; case expr of
                                         (BoolVal True, str)  -> (evalS_maybe s1 str)
                                         (BoolVal False, str) -> (evalS_maybe s2 str)
                                         _                   -> Nothing
evalS_maybe (For var e1 e2 s1) s = do (_, state) <- evalE_maybe (Assignment var e1) s --Problem 6
                                      (_, state') <- evalE_maybe e2 state
                                      (expr1, state1) <- evalE_maybe (BinOp Plus e1 (Val (IntVal 1))) state'
                                      do val <- (evalE_maybe (BinOp GreaterThan e1 e2) state1) ; case val of
                                          (BoolVal True, x)  -> Just x
                                          (BoolVal False, x) -> case (evalS_maybe s1 x) of
                                                  Just x' -> evalS_maybe (For var (Val expr1) e2 s1) x'
                                                  _       -> Nothing
                                          _                  -> Nothing
--Problem 5
newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\s -> Nothing)

--helper functions
getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> (Data.Map.lookup var store >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store))

evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')
evalE_monad (Var x) = getVar x
evalE_monad (Val v) = return v
evalE_monad (Assignment x e) = do expr <- evalE_monad e
                                  var <- setVar x expr; return var

evalS_monad :: Statement -> Imperative ()
evalS_monad w@(While e s1) =  do expr <- (evalE_monad e) ; case expr of
                                      (BoolVal True)  -> do evalS_monad s1
                                      (BoolVal False) -> return ()
                                      _               -> error "Condition must be a BoolVal"
evalS_monad Skip         = return ()
evalS_monad (Sequence s1 s2) = do (evalS_monad s1) ; (evalS_monad s2) ; return ()
evalS_monad (Expr e) = do (evalE_monad e) ; return ()
evalS_monad (If e s1 s2) = do expr <- evalE_monad e ; case expr of
                                (BoolVal True)  -> evalS_monad s1
                                (BoolVal False) -> evalS_monad s2
                                _               -> error "Condition must be a BoolVal" 
evalS_monad (For var e1 e2 s1) = do evalE_monad (Assignment var e1) --Problem 6
                                    val <- evalE_monad (BinOp GreaterThan e1 e2) ; case val of
                                           BoolVal True  -> return ()
                                           BoolVal False -> do evalS_monad s1
                                                               expr'  <- evalE_monad (BinOp Plus e1 (Val(IntVal 1)))
                                                               evalS_monad (For var (Val expr') e2 s1)
                                           _             -> error "Not an Int"
                                           
miniprog :: Imperative Value
miniprog = do
            setVar "x" (IntVal 2)
            setVar "y" (IntVal 3)
            a <- getVar "x"
            b <- getVar "y"
            return (applyOp Plus a b)