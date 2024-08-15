module ParseUtil (untilMatchingParens, ump, (<#>)) where

import Data.Function ((&))
import Types

untilMatchingParens :: String -> Parser String
untilMatchingParens s = ump 1 s (pure "")

-- maybe also give back the remaining string?
ump :: Int -> String -> Parser String -> Parser String
ump 0 _ t = init <$> t
ump _ [] _ = Error "Expected closing parenthesis"
ump n (s : xs) t =
  ump
    ( case s of
        '(' -> n + 1
        ')' -> n - 1
        _ -> n
    )
    xs
    $ (++ [s]) <$> t

(<#>) :: (Functor f) => a -> f (a -> b) -> f b
(<#>) x m = (x &) <$> m