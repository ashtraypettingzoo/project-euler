-- Haskell: https://www.haskell.org/
-- online: https://play.haskell.org/

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.IO.Unsafe

fibs :: [Integer]
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

toDigits :: Integral a => a -> [a]
toDigits x
    | x < 10    = [x]
    | otherwise = toDigits (div x 10) ++ [mod x 10]

fromDigits :: Integral a => [a] -> a
fromDigits = foldr (\x y -> x + 10 * y) 0 . reverse

triangluarNums :: [Int]
triangluarNums = scanl1 (+) [1..]

divisors :: Int -> [Int]
divisors x = [y | y <- [1..x], 0 == mod x y]

properDivisors :: Int -> [Int]
properDivisors = safeInit . divisors
    
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

stringDigits :: String -> [Int]
stringDigits = map (read . (:""))

windows :: Int -> [a] -> [[a]]
windows len = foldr (zipWith (:)) (repeat []) . take len . tails

matrixRot90 :: [[a]] -> [[a]]
matrixRot90 = reverse . transpose

matrixDiags :: [[a]] -> [[a]]
matrixDiags mat = f mat ++ g mat
  where
    f = transpose . zipWith drop [0..]
    g = transpose . map reverse . zipWith take [0..]

matrixAllDiags :: [[a]] -> [[a]]
matrixAllDiags = (on (++) matrixDiags) <*> matrixRot90

factorial :: Integral a => a -> a
factorial x = product [2..x]

split :: Eq a => a -> [a] -> [[a]]
split x xs = case break (x ==) xs of
                  (ys, [])     -> [ys]
                  (ys, (_:zs)) -> ys : split x zs

applyBetween :: (a -> a -> a) -> [a] -> [a]
applyBetween f = concat . map g . safeTail . inits
  where
    g xs = map (f $ last xs) xs

holes :: [a] -> [(a, [a])]
holes []     = []
holes (x:xs) = (x, xs) : (map . second) (x :) (holes xs)

table :: (a -> b -> c) -> [a] -> [b] -> [[c]]
table f ys xs = map (flip map xs . f) ys



p001 :: Int
p001 = sum [x | x <- [0..999], mod x 3 == 0 || mod x 5 == 0]

p002 :: Integer
p002 = sum $ takeWhile (< 4000000) $ filter even $ fibs

p003 :: Int
p003 = maximum $ primeFactors 600851475143

p004 :: Int
p004 = maximum $ filter f $ applyBetween (*) [100..999]
  where
    f = ((==) <*> reverse) . toDigits

p005 :: Int
p005 = foldr1 lcm [1..20]

p006 :: Int
p006 = (sum [1..100])^2 - (sum $ map (^2) [1..100])

p007 :: Int
p007 = primes!!10000

p008 :: IO Int
p008 = do 
    s <- readFile "data/p008.txt"
    return $ f $ stringDigits $ concat $ lines s
  where
    f = maximum . map product . windows 13 

p009 :: Int
p009 = head [a*b*c | a <- [0    ..1000],
                     b <- [a + 1..1000],
                     c <- [b + 1..1000],
                     a + b + c == 1000,
                     a*a + b*b == c*c]

p011 :: IO Int
p011 = do 
    s <- readFile "data/p011.txt"
    return $ f $ map ((map read) . words) $ lines s
  where
    f = maximum . map product . concat . map (windows 4) . g
    g mat = mat ++ transpose mat ++ matrixAllDiags mat

p013 :: IO Integer
p013 = do
    s <- readFile "data/p013.txt"
    return $ f $ map read $ lines s
  where
    f :: [Integer] -> Integer
    f = fromDigits . take 10 . toDigits . sum

p014 :: Int
p014 = fst $ maximumBy (on compare snd) $ map ((,) <*> g) [1..999999]
  where
    f x = bool (3 * x + 1) (div x 2) (even x)
    g = succ . length . takeWhile (1 <) . iterate f

p016 :: Integer
p016 = sum $ toDigits $ 2^1000

p020 :: Integer
p020 = sum $ toDigits $ factorial 100

p021 :: Int
p021 = sum $ filter g [2..9999]
  where
    f = sum . properDivisors
    g x = x /= f x && x == f (f x)

p022 :: IO Int
p022 = do
    s <- readFile "data/p022.txt"
    return $ h $ map g $ sort $ f s
  where
    f = map (init . tail) . split ','
    g = sum . map ((subtract $ ord 'A' - 1) . ord)
    h = sum . zipWith (*) [1..]

p024 :: Int
p024 = fromDigits $ (!! 999999) $ sort $ permutations [0..9]

p025 :: Int
p025 = fromJust $ findIndex (10^999 <=) fibs

p029 :: Int
p029 = length $ nub $ join (liftA2 (^)) [2..100]