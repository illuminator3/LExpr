module PreVerifier (verify) where

import Types
import ParseUtil ((<#>))

verify :: Parser [ParsingElement] -> Parser [ParsingElement]
verify = verifyNext [] PAnd

-- Done -> Previous -> Remaining -> Result
verifyNext :: [ParsingElement] -> ParsingElement -> Parser [ParsingElement] -> Parser [ParsingElement]
verifyNext _ _ (Error e) = Error e
verifyNext done _ (Parser []) = return done
verifyNext done previous (Parser (next : remaining))
  | previous `elem` [PAnd, POr, PXor, PNot] = verifyExpect next [isPVariable, is PNot, isPEnclosed] done remaining
  | otherwise = verifyExpect next [is PAnd, is POr, is PXor] done remaining

isPVariable :: ParsingElement -> Bool
isPVariable (PVariable _) = True
isPVariable _ = False

isPEnclosed :: ParsingElement -> Bool
isPEnclosed (PEnclosed _) = True
isPEnclosed _ = False

is :: ParsingElement -> ParsingElement -> Bool
is = (==)

-- a -> [a -> Bool] ->
-- next expected done remaining
verifyExpect :: ParsingElement -> [ParsingElement -> Bool] -> [ParsingElement] -> [ParsingElement] -> Parser [ParsingElement]
verifyExpect next expected done remaining
  | or $ next <#> expected = verifyNext (done ++ [next]) next (pure remaining)
  | otherwise = Error $ "Unexpected token " ++ show next