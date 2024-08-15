module PreParser (preparse) where

import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import ParseUtil
import Types

parseElemOp :: Char -> Parser ParsingElement
parseElemOp s = case s of
  '!' -> return PNot
  '&' -> return PAnd
  '|' -> return POr
  '^' -> return PXor
  _ -> Error "Unknown operator"

preparse :: String -> Parser [ParsingElement]
preparse "" = return []
preparse (s : xs)
  | s == ' ' = preparse xs
  | s `elem` ['!', '&', '|', '^'] = liftA2 (:) (parseElemOp s) (preparse xs)
  | s == '(' = let rs = untilMatchingParens xs in liftA2 (:) ((rs >>= preparse) <&> PEnclosed) (rs >>= preparse . flip drop xs . (+ 1) . length)
  | otherwise = if isAlphaNum s then fmap ((PVariable $ s : takeWhile isAlphaNum xs) :) . preparse $ dropWhile isAlphaNum xs else Error "Unexpected character"