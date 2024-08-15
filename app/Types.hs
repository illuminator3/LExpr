module Types (Variable, PVariable, Expression (..), Parser (..), ParsingElement (..)) where

type Variable = String

type PVariable = ParsingElement

data Expression = Variable String | And Expression Expression | Or Expression Expression | Xor Expression Expression | Not Expression deriving (Eq)

instance Show Expression where
  show (Variable v) = v
  show (And e1 e2) = "(" ++ show e1 ++ " & " ++ show e2 ++ ")"
  show (Or e1 e2) = "(" ++ show e1 ++ " | " ++ show e2 ++ ")"
  show (Xor e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"
  show (Not e) = "!" ++ show e

data Parser a = Parser a | Error String

instance (Show a) => Show (Parser a) where
  show (Parser c) = show c
  show (Error e) = "Error: " ++ e

instance Monad Parser where
  return = pure
  Error s >>= _ = Error s
  Parser a >>= f = f a

instance Functor Parser where
  fmap f (Parser a) = Parser (f a)
  fmap _ (Error s) = Error s

instance Applicative Parser where
  pure = Parser
  Parser f <*> Parser a = Parser (f a)
  Error s <*> _ = Error s
  _ <*> Error s = Error s

data ParsingElement = PVariable String | PAnd | POr | PXor | PNot | PEnclosed [ParsingElement] deriving (Show, Eq)
