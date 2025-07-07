module Main where

-- HC4T1 - Define a weatherReport Function using pattern matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

main1 :: IO ()
main1 = do
    print $ weatherReport "sunny"     -- Expected: bright day message
    print $ weatherReport "rainy"     -- Expected: umbrella reminder
    print $ weatherReport "windy"     -- Expected: unknown weather


-- HC4T2 - Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
    | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
    | otherwise = "Invalid day"

main2 :: IO ()
main2 = do
    print $ dayType "Sunday"     -- Expected: weekend
    print $ dayType "Wednesday"  -- Expected: weekday
    print $ dayType "Funday"     -- Expected: invalid


-- HC4T3 - Define a gradeComment Function
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent!"
    | grade >= 70 && grade <= 89  = "Good job!"
    | grade >= 50 && grade <= 69  = "You passed."
    | grade >= 0  && grade <= 49  = "Better luck next time."
    | otherwise                   = "Invalid grade"

main3 :: IO ()
main3 = do
    print $ gradeComment 95    -- Expected: Excellent!
    print $ gradeComment 72    -- Expected: Good job!
    print $ gradeComment 30    -- Expected: Better luck next time.
    print $ gradeComment 110   -- Expected: Invalid grade


-- HC4T4 - Rewrite specialBirthday using pattern matching
specialBirthday :: Int -> String
specialBirthday 1 = "First birthday! Happy 1st!"
specialBirthday 18 = "You're an adult now!"
specialBirthday 50 = "Half a century!"
specialBirthday _ = "Just another year."

main4 :: IO ()
main4 = do
    print $ specialBirthday 1    -- Expected: 1st birthday
    print $ specialBirthday 18   -- Expected: adult
    print $ specialBirthday 32   -- Expected: just another year


-- HC4T5 - Catch-All with Age in Message
specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 1  = "First birthday! Happy 1st!"
specialBirthdayWithAge 18 = "You're an adult now!"
specialBirthdayWithAge 50 = "Half a century!"
specialBirthdayWithAge age = "Happy birthday! You are " ++ show age ++ " years old."

main5 :: IO ()
main5 = do
    print $ specialBirthdayWithAge 1    -- Expected: 1st
    print $ specialBirthdayWithAge 50   -- Expected: 50th
    print $ specialBirthdayWithAge 21   -- Expected: generic message with age


-- HC4T6 - Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "The list is empty."
whatsInsideThisList [x] = "The list has one element."
whatsInsideThisList [x, y] = "The list has two elements."
whatsInsideThisList _ = "The list has many elements."

main6 :: IO ()
main6 = do
    print $ whatsInsideThisList ([] :: [Int])        -- Expected: empty
    print $ whatsInsideThisList [42]                 -- Expected: one
    print $ whatsInsideThisList [1, 2]               -- Expected: two
    print $ whatsInsideThisList [1, 2, 3]            -- Expected: many


-- HC4T7 - Ignore Elements in a List
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _ = []

main7 :: IO ()
main7 = do
    print $ firstAndThird [1, 2, 3, 4]     -- Expected: [1,3]
    print $ firstAndThird [5, 6, 7]        -- Expected: [5,7]
    print $ firstAndThird [1]              -- Expected: []


-- HC4T8 - Extract Values from Tuples
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, status) =
    "Name: " ++ name ++ ", Age: " ++ show age ++ ", Status: " ++ show status

main8 :: IO ()
main8 = do
    print $ describeTuple ("Alice", 30, True)   -- Expected: full description
    print $ describeTuple ("Bob", 45, False)
    

-- Combine all main functions
main :: IO ()
main = do
    putStrLn "---- HC4T1 ----"
    main1
    putStrLn "\n---- HC4T2 ----"
    main2
    putStrLn "\n---- HC4T3 ----"
    main3
    putStrLn "\n---- HC4T4 ----"
    main4
    putStrLn "\n---- HC4T5 ----"
    main5
    putStrLn "\n---- HC4T6 ----"
    main6
    putStrLn "\n---- HC4T7 ----"
    main7
    putStrLn "\n---- HC4T8 ----"
    main8





