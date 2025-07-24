-- HC16T1: Reverse a String
reverseString :: String -> String
reverseString = reverse

main :: IO ()
main = do
    let str = "Hello, Haskell!"
    putStrLn $ "Reversed string: " ++ reverseString str

-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

main :: IO ()
main = do
    let str = "madam"
    putStrLn $ "Is palindrome: " ++ show (isPalindrome str)

-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    let num = 5
    putStrLn $ "Factorial of " ++ show num ++ " is: " ++ show (factorial num)

-- HC16T4: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

main :: IO ()
main = do
    let nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    putStrLn $ "Even numbers: " ++ show (filterEven nums)

-- HC16T5: Uppercase String
uppercaseString :: String -> String
uppercaseString = map toUpper

import Data.Char (toUpper)

main :: IO ()
main = do
    let str = "hello, world"
    putStrLn $ "Uppercase string: " ++ uppercaseString str

-- HC16T6: nth Fibonacci Number
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    let n = 6
    putStrLn $ "The " ++ show n ++ "th Fibonacci number is: " ++ show (fibonacci n)

-- HC16T7: Element Existence in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

main :: IO ()
main = do
    let lst = [1, 2, 3, 4, 5]
    let element = 3
    putStrLn $ "Element " ++ show element ++ " exists: " ++ show (elementExists element lst)

-- HC16T8: Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
        | y <= z    = y : z : zs
        | otherwise = z : insert y zs

main :: IO ()
main = do
    let unsortedList = [5, 3, 8, 1, 2, 7]
    putStrLn $ "Sorted list: " ++ show (insertionSort unsortedList)

-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

main :: IO ()
main = do
    let lst = [1, 2, 2, 3, 4, 4, 5]
    putStrLn $ "List without duplicates: " ++ show (removeDuplicates lst)

-- HC16T10: Character Frequency in String
import Data.List (group, sort)

charFrequency :: String -> [(Char, Int)]
charFrequency str = map (\x -> (head x, length x)) . group . sort $ str

main :: IO ()
main = do
    let str = "hello"
    putStrLn $ "Character frequencies: " ++ show (charFrequency str)

