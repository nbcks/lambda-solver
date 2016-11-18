import Data.Char (isAlpha)

data Token = 
    RightBracket 
  | LeftBracket 
  | Text String 
  | Lambda 
  | DOT deriving (Show,Eq)

type Variable = String

data Expr = 
    Atomic Variable
  | Abstract Variable Expr
  | Application Expr Expr
  deriving Show
  
-- monadic Parser

type Parser t a = t -> (a, t)

newtype Parser2 t a = Parser { parse :: t -> (a, t) }

instance Functor (Parser2 t) where
  fmap f p = Parser (\str -> let (out1, rstr) = parse p str in (f out1, rstr))
  
instance Applicative (Parser2 t) where
  pure a = Parser (\str -> (a, str))
  mf <*> p = Parser (\str -> let (f, tstr) = parse mf str in 
                             let (out, tstr') = parse p tstr in 
                             (f out, tstr'))

instance Monad (Parser2 t) where
  m >>= f = Parser (\str -> let (out1, tstr) = parse m str in parse (f out1) tstr)
  return a = Parser (\str -> (a, str))

result :: a -> Parser String a
result a = \x -> (a, x)

($>) :: Parser String a -> (a -> Parser String b) -> Parser String b
m $> f = \str -> let (out1, tstr) = m str in f out1 tstr

-- lexing

lexer :: String -> [Token]
lexer str = let (out, junk) = lexer' str in out

lexer' :: Parser String [Token]
lexer' [] = ([],[])
lexer' (s:ss) = 
  case s of
    '(' -> (lexer' $> (\v -> result (LeftBracket:v))) ss
    ')' -> (lexer' $> (\v -> result (RightBracket:v))) ss
    '\\' -> (lexer' $> (\v -> result (Lambda:v))) ss
    '.' -> (lexer' $> (\v -> result (DOT:v))) ss
    c -> if isAlpha c then (lexer' $> (\v -> result (Text [c]:v))) ss else error $ "expects character, got: " ++ [c]
    
eatString :: Parser String String
eatString (s:ss) = if isAlpha s then (eatString $> (\v -> result (s:v))) ss else ("", (s:ss))
  
-- parsing

{- expr := (\x.expr) | (expr, expr) | variable -}

(&>) :: Parser [Token] a -> (a -> Parser [Token] b) -> Parser [Token] b
m &> f = \str -> let (out1, tstr) = m str in f out1 tstr

result' :: a -> Parser [Token] a
result' a = \x -> (a, x)

parser :: [Token] -> Expr
parser toks = let (e, junk) = parseExpr toks in e

extractString :: Token -> String
extractString (Text c) = if length c > 1 then error "variable bigger than 1: " ++ c else c
extractString _ = error "extractString: not text"

consume :: Parser [Token] Token
consume [] = error "nothing left"
consume (c:cs) = (c, cs)

consumeTok :: Token -> Parser [Token] Token
consumeTok c ss =
  case ss of 
    {[] -> error "empty line";
    (t:ts) -> if t == c then (t, ts) else error ("given: " ++ show t ++ "expected: " ++ show c)}

parseExpr :: Parser [Token] Expr
parseExpr [] = error "nothing"
parseExpr (t:ts) = 
  case t of 
    {Text c -> (Atomic (extractString t), ts);
     LeftBracket -> 
     case head(ts) of -- lookhead to check for Abstract or Application
        {Lambda -> parseAbstract (t:ts);
        _ -> parseApplication (t:ts)};
    x -> error ("parse error: " ++ show x)}

-- (\v.E)
parseAbstract :: Parser [Token] Expr
parseAbstract = 
  consumeTok LeftBracket &> (\_ ->
  consumeTok Lambda &> (\_ -> 
  consume &> (\x ->
  consumeTok DOT &> (\_ -> 
  parseExpr &> (\e -> 
  consumeTok RightBracket &> (\_ -> 
  result' $ Abstract (extractString x) e))))))

-- (E1 E2)
parseApplication :: Parser [Token] Expr
parseApplication = 
  consumeTok LeftBracket &> (\_ -> 
  parseExpr &> (\e1 ->
  parseExpr &> (\e2 -> 
  consumeTok RightBracket &> (\_ -> 
  result' $ Application e1 e2))))


-- evaluator

-- reduction functions take expressions and return smaller ones

-- I/O

main :: IO ()
main = do
  putStrLn "write in lambda expression"
  cs <- getLine
  putStrLn "here is the lexing"
  lexed <- return (lexer cs)
  putStrLn (show $ lexed)
  putStrLn "about to parse"
  parsed <- return $ parser lexed
  putStrLn (show $ parsed)
  
  