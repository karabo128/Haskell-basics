module Main where

-- HC5T1: Apply a function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

main1 :: IO ()
main1 = do
    print $ applyThrice (+1) 5      -- Expected: 8
    print $ applyThrice (*2) 1      -- Expected: 8


-- HC5T2: Filtering odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

main2 :: IO ()
main2 = print oddNumbers            -- Expected: [1,3,5,...,29]


-- HC5T3: Check for uppercase-starting words using `any`
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

main3 :: IO ()
main3 = do
    print $ hasUppercaseStart ["hello", "world"]       -- False
    print $ hasUppercaseStart ["hi", "There"]          -- True


-- HC5T4: Lambda function rewrite
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main4 :: IO ()
main4 = do
    print $ biggerThan10 5     -- False
    print $ biggerThan10 15    -- True


-- HC5T5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

main5 :: IO ()
main5 = do
    print $ multiplyByFive 3   -- 15
    print $ multiplyByFive 7   -- 35


-- HC5T6: Function composition: square then filter even
squaredEvens :: [Int] -> [Int]
squaredEvens = filter even . map (^2)

main6 :: IO ()
main6 = do
    print $ squaredEvens [1..5]    -- [4,16]
    print $ squaredEvens [2,3,4]   -- [4,16]


-- HC5T7: Rewrite using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main7 :: IO ()
main7 = print result    -- Expected: 2*(4+5+6+7+8+9+10) = 2*49 = 98


-- HC5T8: Convert to point-free style
addFive :: Int -> Int
addFive = (+5)

main8 :: IO ()
main8 = do
    print $ addFive 10     -- 15
    print $ addFive (-5)   -- 0


-- HC5T9: Higher-order transformList that applies a function twice
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

main9 :: IO ()
main9 = do
    print $ transformList (+1) [1,2,3]      -- [3,4,5]
    print $ transformList (*2) [1,2,3]      -- [4,8,12]


-- HC5T10: Combine filter, map, any to check if any square > 50
anySquareOver50 :: [Int] -> Bool
anySquareOver50 = any (>50) . map (^2)

main10 :: IO ()
main10 = do
    print $ anySquareOver50 [1,2,3,4,5,6,7]     -- False
    print $ anySquareOver50 [6,7,8,9]           -- True (8^2 = 64)


-- Combined main to run all tasks
main :: IO ()
main = do
    putStrLn "---- HC5T1 ----"
    main1
    putStrLn "\n---- HC5T2 ----"
    main2
    putStrLn "\n---- HC5T3 ----"
    main3
    putStrLn "\n---- HC5T4 ----"
    main4
    putStrLn "\n---- HC5T5 ----"
    main5
    putStrLn "\n---- HC5T6 ----"
    main6
    putStrLn "\n---- HC5T7 ----"
    main7
    putStrLn "\n---- HC5T8 ----"
    main8
    putStrLn "\n---- HC5T9 ----"
    main9
    putStrLn "\n---- HC5T10 ----"
    main10
