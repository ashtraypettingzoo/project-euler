-- Haskell: https://www.haskell.org/
-- online: https://play.haskell.org/

p001 :: Int
p001 = sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

p002 :: Int
p002 = sum $ takeWhile (< 4000000) $ filter even fibs
  where
    fibs :: [Int]
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

p009 :: Int
p009 = head [a*b*c | a <- [0    ..1000],
                     b <- [a + 1..1000],
                     c <- [b + 1..1000],
                     a + b + c == 1000,
                     a*a + b*b == c*c]
