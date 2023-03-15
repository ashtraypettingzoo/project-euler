-- Haskell: https://www.haskell.org/
-- online: https://play.haskell.org/

p001 = sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]
