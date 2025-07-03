-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main1 :: IO ()
main1 = do
    print $ doubleThenIncrement 3  -- Expected: 7

-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

main2 :: IO ()
main2 = do
    print $ circleArea 5  -- Expected: 78.5398...

-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main3 :: IO ()
main3 = do
    print $ greaterThan18 20  -- Expected: True
    print $ greaterThan18 15  -- Expected: False

-- HC1T4 - Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . quicksort
    where quicksort [] = []
          quicksort (x:xs) =
              let smaller = quicksort [y | y <- xs, snd y <= snd x]
                  larger  = quicksort [y | y <- xs, snd y > snd x]
              in  smaller ++ [x] ++ larger

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main4 :: IO ()
main4 = do
    let players = [("Alice", 30), ("Bob", 20), ("Charlie", 50), ("Daisy", 40), ("Eve", 10)]
    print $ getTopThreePlayers players -- Expected: ["Charlie", "Daisy", "Alice"]

-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

main5 :: IO ()
main5 = do
    print $ take 10 infiniteNumbers  -- Expected: [1,2,3,4,5,6,7,8,9,10]

-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main6 :: IO ()
main6 = do
    print $ addNumbers 5 7  -- Expected: 12

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Fractional a => a -> a
fToC f = (f - 32) * 5 / 9

main7 :: IO ()
main7 = do
    print $ fToC 98.6  -- Expected: ~37.0

-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main8 :: IO ()
main8 = do
    print $ applyTwice (+1) 3  -- Expected: 5
    print $ applyTwice (*2) 4  -- Expected: 16

-- Optional: Run all mains
main :: IO ()
main = do
    putStrLn "Task 1:"
    main1
    putStrLn "\nTask 2:"
    main2
    putStrLn "\nTask 3:"
    main3
    putStrLn "\nTask 4:"
    main4
    putStrLn "\nTask 5:"
    main5
    putStrLn "\nTask 6:"
    main6
    putStrLn "\nTask 7:"
    main7
    putStrLn "\nTask 8:"
    main8
