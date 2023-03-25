-- Haskell: https://www.haskell.org/
-- online: https://play.haskell.org/

import Control.Applicative
import Data.List

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primeFactors :: Int -> [Int]
primeFactors x = f x primes
  where
    f x (y:ys)
      | x < 2        = []
      | x < y^2      = [x]
      | 0 == mod x y = y : f (div x y) (y:ys)
      | otherwise    = f x ys

isPrime :: Int -> Bool
isPrime x = 1 == (length $ primeFactors x)

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

toDigits :: Int -> [Int]
toDigits x
    | x < 10    = [x]
    | otherwise = toDigits (div x 10) ++ [mod x 10]

fromDigits :: [Int] -> Int
fromDigits = foldr (\x y -> x + 10 * y) 0 . reverse

triangluarNums :: [Int]
triangluarNums = scanl1 (+) [1..]

factors :: Int -> [Int]
factors x = [y | y <- [1..x], 0 == mod x y]


p001 :: Int
p001 = sum [x | x <- [0..999], mod x 3 == 0 || mod x 5 == 0]

p002 :: Int
p002 = sum $ takeWhile (< 4000000) $ filter even fibs

p003 :: Int
p003 = maximum $ primeFactors 600851475143

p004 :: Int
p004 = maximum $ filter f $ liftA2 (*) xs xs
  where
    xs = [100..999]
    f = ((==) <*> reverse) . toDigits

p005 :: Int
p005 = foldr1 lcm [1..20]

p006 :: Int
p006 = (sum [1..100])^2 - (sum $ map (^2) [1..100])

p007 :: Int
p007 = primes!!10000

p009 :: Int
p009 = head [a*b*c | a <- [0    ..1000],
                     b <- [a + 1..1000],
                     c <- [b + 1..1000],
                     a + b + c == 1000,
                     a*a + b*b == c*c]
