module Main where

-- HC6T1: Recursive factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = do
    print $ factorial 5   -- 120
    print $ factorial 0   -- 1


-- HC6T2: Recursive Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = do
    print $ fibonacci 0   -- 0
    print $ fibonacci 5   -- 5
    print $ fibonacci 8   -- 21


-- HC6T3: Sum of list using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

main3 :: IO ()
main3 = do
    print $ sumList [1, 2, 3, 4]  -- 10


-- HC6T4: Product of list using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

main4 :: IO ()
main4 = do
    print $ productList [1, 2, 3, 4]  -- 24


-- HC6T5: Reverse list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main5 :: IO ()
main5 = do
    print $ reverseList [1,2,3,4]   -- [4,3,2,1]


-- HC6T6: Check if element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
    | x == y    = True
    | otherwise = elementExists y xs

main6 :: IO ()
main6 = do
    print $ elementExists 3 [1,2,3,4]  -- True
    print $ elementExists 5 [1,2,3,4]  -- False


-- HC6T7: Length of a list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

main7 :: IO ()
main7 = do
    print $ listLength [1,2,3,4]  -- 4
    print $ listLength []        -- 0


-- HC6T8: Filter even numbers from list
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
    | even x    = x : filterEvens xs
    | otherwise = filterEvens xs

main8 :: IO ()
main8 = do
    print $ filterEvens [1..10]  -- [2,4,6,8,10]


-- HC6T9: Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

main9 :: IO ()
main9 = do
    print $ myMap (+1) [1,2,3]    -- [2,3,4]
    print $ myMap (*2) [4,5,6]    -- [8,10,12]


-- HC6T10: Recursive digits of a number
digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main10 :: IO ()
main10 = do
    print $ digits 12345   -- [1,2,3,4,5]
    print $ digits 0       -- [0]


-- Combine all main functions
main :: IO ()
main = do
    putStrLn "---- HC6T1 ----"
    main1
    putStrLn "\n---- HC6T2 ----"
    main2
    putStrLn "\n---- HC6T3 ----"
    main3
    putStrLn "\n---- HC6T4 ----"
    main4
    putStrLn "\n---- HC6T5 ----"
    main5
    putStrLn "\n---- HC6T6 ----"
    main6
    putStrLn "\n---- HC6T7 ----"
    main7
    putStrLn "\n---- HC6T8 ----"
    main8
    putStrLn "\n---- HC6T9 ----"
    main9
    putStrLn "\n---- HC6T10 ----"
    main10
