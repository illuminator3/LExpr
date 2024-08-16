Example:
```
ghci> Enter expression: a ^ b | (b & !(c | !a ^ b)) ^ c        
Parsed: (a ^ ((b | (b & !((c | !a) ^ b))) ^ c))
Minified: (a ^ (b ^ c))
Done
```
