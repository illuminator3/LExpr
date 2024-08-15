module Generator (minify) where

import Data.List (group, sort)
import Parser (parseDown)
import Types

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

findVars :: Expression -> [Variable]
findVars = rmdups . findVarsDup

findVarsDup :: Expression -> [Variable]
findVarsDup expr = case expr of
  Variable v -> [v]
  Not e -> findVars e
  And e1 e2 -> findVars e1 ++ findVars e2
  Or e1 e2 -> findVars e1 ++ findVars e2
  Xor e1 e2 -> findVars e1 ++ findVars e2

evaluate :: Expression -> (Variable -> Bool) -> Bool
evaluate expr fev = case expr of
  Variable v -> fev v
  Not e -> not (evaluate e fev)
  And e1 e2 -> evaluate e1 fev && evaluate e2 fev
  Or e1 e2 -> evaluate e1 fev || evaluate e2 fev
  Xor e1 e2 -> evaluate e1 fev /= evaluate e2 fev

nbools :: Int -> [[Bool]]
nbools 0 = [[]]
nbools n = [False : bs | bs <- l] ++ [True : bs | bs <- l] where l = nbools (n - 1)

createLookup :: [Variable] -> [Bool] -> (Variable -> Bool)
createLookup vars bools v = bools !! indexOf vars v

indexOf :: (Eq a) => [a] -> a -> Int
indexOf [] _ = -1
indexOf (x : xs) e
  | x == e = 0
  | otherwise = 1 + indexOf xs e

evaluateAll :: Expression -> [Variable] -> [Bool]
evaluateAll expr vars = map (evaluate expr . (createLookup vars)) (nbools (length vars))

-- \bools -> evaluate expr (createLookup vars bools) :: [Bool] -> Bool
-- evaluate expr :: (Variable -> Bool) -> Bool
-- createLookup vars :: [Bool] -> (Variable -> Bool)
-- f :: [a] -> b
-- g :: b -> c
-- für alle nbools erzeuge lookup und dann evaluate

areTheSame :: Expression -> Expression -> Bool
areTheSame e1 e2
  | findVars e1 == findVars e2 = let vars = findVars e1 in evaluateAll e1 vars == evaluateAll e2 vars
  | otherwise = False

genMatchingPossibilitiesFixedN :: Expression -> Int -> [Parser Expression]
genMatchingPossibilitiesFixedN expr n = filter (pareTheSame expr) (genPossibilitiesFixedN (map PVariable (findVars expr)) n)

pareTheSame :: Expression -> Parser Expression -> Bool
pareTheSame _ (Error _) = False
pareTheSame expr (Parser e) = areTheSame expr e

justgenMatchingPossibilitiesFixedN :: Expression -> Int -> [Expression]
justgenMatchingPossibilitiesFixedN expr = unwrap . genMatchingPossibilitiesFixedN expr

unwrap :: [Parser a] -> [a]
unwrap [] = []
unwrap ((Error _) : ps) = unwrap ps
unwrap ((Parser p) : ps) = p : unwrap ps

apOp :: [PVariable] -> Int -> [ParsingElement]
-- r = remaining
-- r = 0: []
-- r = 1: variable
-- r = 2: not
-- r = 3, 4: not, variable
-- r >= 5: not, variable, penclosed 3, penclosed 5, ..., penclosed (r - 2)
apOp vars r = case r of
  0 -> []
  1 -> vars
  2 -> [PNot]
  3 -> PNot : vars
  4 -> PNot : vars
  _ -> PNot : vars ++ (flatMap [[PEnclosed cl | cl <- (step vars k 0 PAnd)] | k <- [3 .. r - 2], odd k])

flatMap :: [[a]] -> [a]
flatMap = concat

-- TODO could maybe shortcut here because of double negation, would have to implement a stop signal (monad)
-- apNot :: [PVariable] -> Int -> [ParseElement]
-- r = 0: []
-- r = 1: variable
-- r = 2: not
-- r = 3: not, varianle
-- apNot vars r =

allPossible :: [PVariable] -> Int -> Int -> ParsingElement -> [ParsingElement]
allPossible vars size n prev = case prev of
  PVariable _ -> [POr, PXor, PAnd]
  PEnclosed _ -> [POr, PXor, PAnd]
  POr -> apOp vars (size - n)
  PAnd -> apOp vars (size - n)
  PXor -> apOp vars (size - n)
  PNot -> apOp vars (size - n)

-- # Total Length -> Current Step -> Last Char -> Result
step :: [PVariable] -> Int -> Int -> ParsingElement -> [[ParsingElement]]
step vars size n prev
  | n == size = [[]]
  | otherwise = let possible = allPossible vars size n prev in flatMap [[poss : next | next <- step vars size (n + 1) poss] | poss <- possible]

genPossibilitiesFixedN :: [PVariable] -> Int -> [Parser Expression]
genPossibilitiesFixedN vars n = map (parseDown . pure) $ step vars n 0 PAnd

-- takes at least length(expr) steps
findSmallestMatchingGeq :: Int -> Expression -> Expression
findSmallestMatchingGeq n expr
  | null (justgenMatchingPossibilitiesFixedN expr n) = findSmallestMatchingGeq (n + 1) expr
  | otherwise = head (justgenMatchingPossibilitiesFixedN expr n)

minify :: Expression -> Expression
minify = findSmallestMatchingGeq 0
