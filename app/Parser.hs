module Parser (parse, parseDown) where

import PreParser
import PreVerifier
import Types

parse :: String -> Parser Expression
parse = parseDown . verify . preparse

parseDown :: Parser [ParsingElement] -> Parser Expression
parseDown (Error e) = Error e
parseDown (Parser []) = Error "Unexpected end of input in parseDown"
parseDown (Parser [PVariable s]) = pure (Variable s)
parseDown (Parser [PEnclosed e]) = parseDown (pure e)
parseDown (Parser (x : xs))
  | containsOp (x : xs) = parseDownNext [] (pure (x : xs))
  | x == PNot = Not <$> parseDown (pure xs)
  | otherwise = Error "What happened here"

containsOp :: [ParsingElement] -> Bool
containsOp = elemAny [POr, PXor, PAnd]

-- checks if any element of l1 is contained in l2
elemAny :: (Eq a) => [a] -> [a] -> Bool
elemAny l1 l2 = any (`elem` l2) l1

parseDownNext :: [ParsingElement] -> Parser [ParsingElement] -> Parser Expression
parseDownNext _ (Error e) = Error e
parseDownNext _ (Parser []) = Error "Unexpected end of input in parseDownNext"
parseDownNext done (Parser (next : remaining)) = if isHighestOrder next done remaining then createOpExp next (parseDown $ pure done) (parseDown $ pure remaining) else parseDownNext (done ++ [next]) (Parser remaining)

createOpExp :: ParsingElement -> Parser Expression -> Parser Expression -> Parser Expression
createOpExp _ (Error e) _ = Error e
createOpExp _ _ (Error e) = Error e
createOpExp POr (Parser a) (Parser b) = Parser (Or a b)
createOpExp PXor (Parser a) (Parser b) = Parser (Xor a b)
createOpExp PAnd (Parser a) (Parser b) = Parser (And a b)
createOpExp _ _ _ = Error "Invalid operator"

isHighestOrder :: ParsingElement -> [ParsingElement] -> [ParsingElement] -> Bool
isHighestOrder p d r = isHighestOrderIn p (d ++ [p] ++ r)

isHighestOrderIn :: ParsingElement -> [ParsingElement] -> Bool
isHighestOrderIn PAnd l = PAnd `elem` l
isHighestOrderIn PXor l = (not $ isHighestOrderIn PAnd l) && PXor `elem` l
isHighestOrderIn POr l = not (isHighestOrderIn PAnd l || (PXor `elem` l)) && POr `elem` l
isHighestOrderIn _ _ = False