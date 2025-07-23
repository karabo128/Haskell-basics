-- HC12T1: Print a Welcome Message
main :: IO ()
main = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

main :: IO ()
main = do
    let sum = addTwoNumbers 3 5
    print sum

-- HC12T3: Factorial Function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    print $ factorial 5

-- HC12T4: First 10 Fibonacci Numbers
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    let fibs = [fib n | n <- [0..9]]
    print fibs

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    if isPalindrome input
        then putStrLn "It's a palindrome!"
        else putStrLn "It's not a palindrome."

-- HC12T6: Sort a List of Integers
main :: IO ()
main = do
    putStrLn "Enter a list of integers separated by spaces:"
    input <- getLine
    let numbers = map read (words input) :: [Int]
    let sorted = sort numbers
    print sorted

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Float -> Float
calculateCircleArea radius = pi * radius * radius

main :: IO ()
main = do
    let area = calculateCircleArea 5.0
    print area

-- HC12T8: Merge Two Sorted Lists
mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x < y     = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

main :: IO ()
main = do
    let merged = mergeLists [1, 3, 5] [2, 4, 6]
    print merged

-- HC12T9: Read and Print File Content
import System.IO

main :: IO ()
main = do
    content <- readFile "example.txt"
    putStrLn content

-- HC12T10: Mathematical Operations Module
module MathOperations where

add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

main :: IO ()
main = do
    print $ add 3 5
    print $ multiply 3 5
