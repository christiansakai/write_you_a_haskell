module NanoParsec where

import Data.Char (isDigit)
import Control.Monad 
  ( MonadPlus
  , mzero
  , mplus
  , forever
  )
import Control.Applicative 
  ( Alternative
  , empty
  , (<|>)
  , some
  , many
  )

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser (Parser parse) str =
  case parse str of
    [(result, [])]    -> result
    [(_, remainder)]  -> error "Parser did not consume the entire stream."
    _                 -> error "Parser error."

runParserDebug :: Parser a -> String -> [(a, String)]
runParserDebug (Parser parse) str = parse str


-- Composition Laws

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser parse) = 
    Parser $ \str -> [(f a, str') | (a, str') <- parse str ]

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser $ \str -> [(a, str)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser parseF <*> Parser parseA = 
    Parser $ \str ->
      [ (f a, str'')
      | (f, str') <- parseF str
      , (a, str'') <- parseA str'
      ]

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \str -> []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  Parser parse <|> Parser parse' =
    Parser $ \str ->
      case parse str of
        []      -> parse' str
        result  -> result

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser parseA >>= f =
    Parser $ \str ->
      let listOfAStr' = parseA str
          listOfBStr' = [ parse (f a) str'
                        | (a, str') <- listOfAStr' ]
       in concat listOfBStr'

instance MonadPlus Parser where
  -- mzero :: Parser a
  mzero = empty

  -- mplus :: Parser a -> Parser a -> Parser a
  Parser parse `mplus` Parser parse' =
    Parser $ \str -> parse str ++ parse' str


-- Base Combinators

item :: Parser Char
item = Parser $ \str ->
  case str of
    []      -> []
    (c:cs)  -> [(c, cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= checkSatisfiy
  where 
    checkSatisfiy char =
      case f char of
        True -> pure char
        _    -> empty

-- |
-- Recursive left descent grammar
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
operandParser `chain` operatorParser = do
  operandA <- operandParser
  rest operandA
    
  where
    rest operandA = (do
      operator <- operatorParser
      operandB <- operandParser
      rest (operator operandA operandB)
      ) <|> return operandA


-- Actual Combinators

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (\char -> char `elem` cs)

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

spaces :: Parser String
spaces = many $ oneOf " \n\r"

natural :: Parser Integer
natural = fmap read $ some (satisfy isDigit)

string :: String -> Parser String
string []     = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

token :: Parser a -> Parser a
token parser = do
  tok <- parser
  spaces
  return tok

reserved :: String -> Parser String
reserved s = token (string s)

number :: Parser Int
number = do
  sign <- string "-" <|> return []
  digits <- some digit
  return $ read (sign ++ digits)

parentheses :: Parser a -> Parser a
parentheses parser = do
  reserved "("
  thing <- parser
  reserved ")"
  return thing


-- |
-- Backus Naur Form
-- for Calculator grammar
-- number = [ "-" ] digit { digit }
-- digit  = "0" | ... | "9"
-- expr   = term { addsubop term }
-- term   = factor { mullop factor }
-- factor = "(" expr ")" | number
-- addsubop = "+" | "-"
-- mullop   = "*"

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval (Add expr1 expr2)  = eval expr1 + eval expr2
eval (Sub expr1 expr2)  = eval expr1 - eval expr2
eval (Mul expr1 expr2)  = eval expr1 * eval expr2
eval (Lit i)            = i

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

expr :: Parser Expr
expr = term `chain` addSubOp

term :: Parser Expr
term = factor `chain` mullOp

factor :: Parser Expr
factor = int <|> parentheses expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp operatorSymbol operatorFunc = do
  reserved operatorSymbol
  return operatorFunc

addSubOp :: Parser (Expr -> Expr -> Expr)
addSubOp =
  (infixOp "+" Add) <|> (infixOp "-" Sub)

mullOp :: Parser (Expr -> Expr -> Expr)
mullOp = infixOp "*" Mul

run :: String -> Expr
run string = runParser expr string

runDebug :: String -> [(Expr, String)]
runDebug string = runParserDebug expr string

main :: IO ()
main = forever $ do
  putStr "> "
  line <- getLine
  print $ eval $ run line

