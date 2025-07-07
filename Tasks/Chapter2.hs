-- HC2T1 - Type Checking

main1 :: IO ()
main1 = do
    print (42 :: Int)              -- Output: 42
    print (3.14 :: Double)            -- Output: 3.14
    print ("Haskell"  :: String)       -- Output: "Haskell"
    print ('Z' :: Char)             -- Output: 'Z'
    print (True && False)   -- Output: False

-- HC2T2 - Function Signatures and Implementations

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings a b = a ++ b

main2 :: IO ()
main2 = do
    print $ add 3 4                    -- Output: 7
    print $ isEven 6                  -- Output: True
    print $ concatStrings "Hi, " "Bob" -- Output: "Hi, Bob"

-- HC2T3 - Immutable Variables

myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main3 :: IO ()
main3 = do
    print myAge          -- Output: 25
    print piValue        -- Output: 3.14159
    print greeting       -- Output: "Hello, Haskell!"
    print isHaskellFun   -- Output: True

    -- Uncommenting the line below would cause a compile-time error (immutability)
    -- myAge = 30
    
-- HC2T4 - Infix and Prefix Conversion

main4 :: IO ()
main4 = do
    -- Infix to Prefix
    print $ (+) 5 3             -- Output: 8
    print $ (*) 10 4            -- Output: 40
    print $ (&&) True False     -- Output: False

    -- Prefix to Infix
    print $ 7 + 2               -- Output: 9
    print $ 6 * 5               -- Output: 30
    print $ True && False       -- Output: False
    
-- HC2T5 - Define Functions

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

main5 :: IO ()
main5 = do
    print $ circleArea 3.0          -- Output: 28.274334
    print $ maxOfThree 10 25 13     -- Output: 25

-- HC2T6 - Int vs Integer

smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

main6 :: IO ()
main6 = do
    print smallNumber             -- Large Int within bounds
    print bigNumber               -- Huge Integer
    print (2^64 :: Int)           -- Might overflow on some systems

-- HC2T7 - Boolean Expressions

main7 :: IO ()
main7 = do
    print (True && True)       -- Output: True
    print (False || False)     -- Output: False
    print (not False)          -- Output: True
    print (5 > 10)             -- Output: False
