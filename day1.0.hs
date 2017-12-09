import Data.Char

breakApart :: String -> [Int]
breakApart s = map digitToInt s

zeroUnlessSame :: Int -> Int -> Int
zeroUnlessSame a b 
 | a == b = a
 | otherwise = 0
 
second :: [Int] -> Int
second [] = 0
second [_] = 0
second xs = head $ tail xs

nextTwo :: [Int] -> [Int]
nextTwo xs = head xs : second xs : []

foo :: [Int] -> [Int] -> [Int]
foo [] output = output
foo input output = foo (tail input) ((zeroUnlessSame (head $ nextTwo input) (second $ nextTwo input)) : output)

addFirstToEnd :: [a] -> [a]
addFirstToEnd [] = []
addFirstToEnd xs = xs ++ [head xs]

solve :: String -> Int
solve s = sum (foo (addFirstToEnd (breakApart s)) [])

