module Main where
import Text.Read (readMaybe)

-- HC7T1: Define Color and implement Eq
data Color = Red | Green | Blue deriving (Show, Read, Enum, Bounded)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

main1 :: IO ()
main1 = do
    print $ Red == Red     -- True
    print $ Red == Blue    -- False


-- HC7T2: Implement Ord for Color: Red < Green < Blue
instance Ord Color where
    compare Red Green   = LT
    compare Red Blue    = LT
    compare Green Blue  = LT
    compare Green Red   = GT
    compare Blue Red    = GT
    compare Blue Green  = GT
    compare _ _         = EQ

main2 :: IO ()
main2 = do
    print $ Red < Green    -- True
    print $ Blue > Green   -- True
    print $ Red >= Red     -- True


-- HC7T3: Function with Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

main3 :: IO ()
main3 = do
    print $ compareValues 10 20    -- 20
    print $ compareValues 'z' 'a'  -- 'z'


-- HC7T4: Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double deriving (Eq)

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input = 
        case words input of
            ["Circle", r] -> [(Circle (read r), "")]
            ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
            _ -> []

main4 :: IO ()
main4 = do
    print $ Circle 5
    print $ Rectangle 3 4
    print $ (read "Circle 10" :: Shape)


-- HC7T5: squareArea with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

main5 :: IO ()
main5 = do
    print $ squareArea 4       -- 16
    print $ squareArea 4.5     -- 20.25


-- HC7T6: circleCircumference using Integral and Floating
circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r

main6 :: IO ()
main6 = do
    print $ circleCircumference 3      -- 18.849...
    print $ circleCircumference 10.5   -- 65.973...


-- HC7T7: nextColor using Enum and Bounded
nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound
    | otherwise     = succ c

main7 :: IO ()
main7 = do
    print $ nextColor Red    -- Green
    print $ nextColor Blue   -- Red


-- HC7T8: Use Read to parse Shape safely
parseShape :: String -> Maybe Shape
parseShape input = readMaybe input :: Maybe Shape

main8 :: IO ()
main8 = do
    print $ parseShape "Circle 10"           -- Just (Circle 10.0)
    print $ parseShape "Rectangle 3 4"       -- Just (Rectangle 3.0 4.0)
    print $ parseShape "Triangle 3 4 5"      -- Nothing


-- HC7T9: Describable type class
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "Yes"
    describe False = "No"

instance Describable Shape where
    describe (Circle r) = "A circle of radius " ++ show r
    describe (Rectangle w h) = "A rectangle of width " ++ show w ++ " and height " ++ show h

main9 :: IO ()
main9 = do
    print $ describe True
    print $ describe (Rectangle 5 6)


-- HC7T10: describeAndCompare with constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (if x >= y then x else y)

-- Use Shape for testing since Bool does not have Ord instance
main10 :: IO ()
main10 = do
    let s1 = Circle 3
    let s2 = Circle 5
    print $ describeAndCompare s1 s2  -- Describes the larger circle

-- Combine all main functions
main :: IO ()
main = do
    putStrLn "---- HC7T1 ----"
    main1
    putStrLn "\n---- HC7T2 ----"
    main2
    putStrLn "\n---- HC7T3 ----"
    main3
    putStrLn "\n---- HC7T4 ----"
    main4
    putStrLn "\n---- HC7T5 ----"
    main5
    putStrLn "\n---- HC7T6 ----"
    main6
    putStrLn "\n---- HC7T7 ----"
    main7
    putStrLn "\n---- HC7T8 ----"
    main8
    putStrLn "\n---- HC7T9 ----"
    main9
    putStrLn "\n---- HC7T10 ----"
    main10
