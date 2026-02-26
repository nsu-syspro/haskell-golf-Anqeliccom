module Task1 where

-- | Compresses given data using run-length encoding.
--
-- Usage example:
--
-- >>> encode "aaabbccaadaaa"
-- [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- >>> encode "abc"
-- [(1,'a'),(1,'b'),(1,'c')]
-- >>> encode []
-- []
--
encode :: Eq a => [a] -> [(Int, a)]
encode = foldr f []
  where
    f x ((n, y):t)
      | x == y = (n + 1, x) : t
    f x t = (1, x) : t

-- | Decompresses given data using run-length decoding.
--
-- Usage example:
--
-- >>> decode [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- "aaabbccaadaaa"
-- >>> decode [(1,'a'),(1,'b'),(1,'c')]
-- "abc"
-- >>> decode []
-- []
--
decode :: [(Int, a)] -> [a]
decode = concatMap f
  where
    f (0, _) = []
    f (n, x) = x : f (n - 1, x)

-- | Rotates given finite list to the left for a given amount N
--
-- If N is negative, then rotates to the right instead.
--
-- Usage example:
--
-- >>> rotate 3 "abcdefgh"
-- "defghabc"
-- >>> rotate (-2) "abcdefgh"
-- "ghabcdef"
-- >>> rotate 0 "abcdefgh"
-- "abcdefgh"
-- >>> rotate 5 "abc"
-- "cab"
-- >>> rotate 5 ""
-- ""
--
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n x = drop k x ++ take k x
  where 
    k = n `mod` length x
