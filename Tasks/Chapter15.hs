-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation
import System.IO
import Control.Exception
import Text.Read (readMaybe)

-- A safe read function for reading numbers from file
readVelocity :: String -> IO (Maybe Double)
readVelocity filename = do
    result <- try (readFile filename) :: IO (Either IOException String)
    case result of
        Left _  -> return Nothing  -- File reading failed
        Right content -> return (readMaybe content)  -- Parse velocity

calculateVelocity :: Double -> Double -> Double
calculateVelocity distance time = distance / time

main :: IO ()
main = do
    let filename = "velocity.txt"
    velocity <- readVelocity filename
    case velocity of
        Nothing -> putStrLn "Error reading velocity from file or invalid format."
        Just v  -> print $ calculateVelocity 100 v

-- HC15T2: Self-Driving AI Car System
data TrafficLight = Red | Green | Yellow deriving (Show, Read)

-- Basic AI car system that reacts to traffic light colors
selfDrivingCar :: TrafficLight -> String
selfDrivingCar Red     = "Stop"
selfDrivingCar Green   = "Go"
selfDrivingCar Yellow  = "Slow down"

main :: IO ()
main = do
    let light = Green
    putStrLn (selfDrivingCar light)

-- HC15T3: Custom Exception for Traffic Light Errors
import Control.Exception

data TrafficLightError = InvalidTrafficLight String
    deriving Show

instance Exception TrafficLightError

data TrafficLight = Red | Green | Yellow deriving (Show, Read)

-- Throw an error if the traffic light is invalid
validateTrafficLight :: String -> IO TrafficLight
validateTrafficLight "Red"     = return Red
validateTrafficLight "Green"   = return Green
validateTrafficLight "Yellow"  = return Yellow
validateTrafficLight invalid   = throwIO (InvalidTrafficLight invalid)

main :: IO ()
main = do
    result <- try (validateTrafficLight "Blue") :: IO (Either TrafficLightError TrafficLight)
    case result of
        Left (InvalidTrafficLight err) -> putStrLn ("Error: " ++ err)
        Right light -> print light

-- HC15T4: Exception Handler for Traffic Light
import Control.Exception

data TrafficLight = Red | Yellow | Green
  deriving (Show)

data TrafficLightError = InvalidTrafficLight String
  deriving Show

instance Exception TrafficLightError

validateTrafficLight :: String -> IO TrafficLight
validateTrafficLight "Red"    = return Red
validateTrafficLight "Green"  = return Green
validateTrafficLight "Yellow" = return Yellow
validateTrafficLight invalid  = throwIO (InvalidTrafficLight invalid)

main :: IO ()
main = handle handler $ do
    validateTrafficLight "Blue"
    putStrLn "Traffic light is valid"

handler :: TrafficLightError -> IO ()
handler (InvalidTrafficLight err) = putStrLn ("Caught error: " ++ err)


-- HC15T5: Safe Division Using Maybe
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing  -- Division by zero
safeDivide x y = Just (x / y)

main :: IO ()
main = do
    let result = safeDivide 10 2
    case result of
        Nothing -> putStrLn "Cannot divide by zero"
        Just value -> print value

-- HC15T6: Safe Input Parsing with readMaybe
import Text.Read (readMaybe)

parseInput :: String -> Maybe Double
parseInput = readMaybe

main :: IO ()
main = do
    let input = "123.45"  
    putStrLn $ "Simulated input: " ++ input
    case parseInput input of
        Nothing -> putStrLn "Invalid input!"
        Just value -> print value


-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
import Text.Read (readMaybe)

calculateVelocity :: Double -> Double -> Maybe Double
calculateVelocity distance time
    | time == 0 = Nothing
    | otherwise = Just (distance / time)

main :: IO ()
main = do
    let distanceInput = "100"  
        timeInput = "0"        

    putStrLn $ "Distance input: " ++ distanceInput
    putStrLn $ "Time input: " ++ timeInput

    let maybeDistance = readMaybe distanceInput :: Maybe Double
        maybeTime = readMaybe timeInput :: Maybe Double

    case (maybeDistance, maybeTime) of
        (Just dist, Just t) -> case calculateVelocity dist t of
            Nothing -> putStrLn "Time cannot be zero."
            Just velocity -> print velocity
        _ -> putStrLn "Invalid input for distance or time."


        -- HC15T8: Division with Either for Detailed Errors
safeDivide :: Double -> Double -> Either String Double
safeDivide _ 0 = Left "Division by zero error!"
safeDivide x y = Right (x / y)

main :: IO ()
main = do
    let result = safeDivide 10 0
    case result of
        Left err -> putStrLn err
        Right value -> print value

-- HC15T9: Try Function for File IO Exceptions
import Control.Exception
import System.IO

main :: IO ()
main = do
    result <- try (readFile "nonexistentfile.txt") :: IO (Either IOException String)
    case result of
        Left err -> putStrLn $ "Caught exception: " ++ show err
        Right content -> putStrLn $ "File content: " ++ content

-- HC15T10: Hybrid Error Handling with Either and IO
import Control.Exception
import Text.Read (readMaybe)

safeRead :: String -> IO (Either String Double)
safeRead str = return $ maybe (Left "Invalid number") Right (readMaybe str)

main :: IO ()
main = do
    putStrLn "Enter distance:"
    distanceInput <- getLine
    putStrLn "Enter time:"
    timeInput <- getLine

    eitherDistance <- safeRead distanceInput
    eitherTime <- safeRead timeInput

    case (eitherDistance, eitherTime) of
        (Right dist, Right time) -> 
            if time == 0
                then putStrLn "Time cannot be zero."
                else print (dist / time)
        (Left err, _) -> putStrLn ("Error: " ++ err)
        (_, Left err) -> putStrLn ("Error: " ++ err)

