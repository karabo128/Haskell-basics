-- HC11T1: Greet the User
main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

-- HC11T2: Count Characters in a Line
main :: IO ()
main = do
    putStrLn "Enter a line:"
    input <- getLine
    let lengthOfInput = length input
    putStrLn ("Number of characters: " ++ show lengthOfInput)

-- HC11T3: Double a Number
main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    putStrLn ("Double the number: " ++ show (number * 2))

-- HC11T4: Concatenate Two Lines
main :: IO ()
main = do
    putStrLn "Enter the first line:"
    line1 <- getLine
    putStrLn "Enter the second line:"
    line2 <- getLine
    putStrLn ("Concatenated result: " ++ line1 ++ line2)

-- HC11T5: Repeat Until "quit"
main :: IO ()
main = do
    putStrLn "Enter a command:"
    command <- getLine
    if command == "quit"
        then putStrLn "Goodbye!"
        else do
            putStrLn ("You entered: " ++ command)
            main

-- HC11T6: Uppercase Converter
import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "Enter a line:"
    input <- getLine
    let uppercased = map toUpper input
    putStrLn ("Uppercase: " ++ uppercased)

-- HC11T7: User Options
main :: IO ()
main = do
    putStrLn "Choose an option:"
    putStrLn "1. Greet"
    putStrLn "2. Exit"
    choice <- getLine
    case choice of
        "1" -> putStrLn "Hello, User!"
        "2" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid choice!"

-- HC11T8: Even or Odd Checker
main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    if even number
        then putStrLn "The number is even."
        else putStrLn "The number is odd."

-- HC11T9: Sum Two Numbers
main :: IO ()
main = do
    putStrLn "Enter the first number:"
    num1 <- getLine
    putStrLn "Enter the second number:"
    num2 <- getLine
    let sumOfNumbers = (read num1 :: Int) + (read num2 :: Int)
    putStrLn ("The sum is: " ++ show sumOfNumbers)

-- HC11T10: Reverse User Input
main :: IO ()
main = do
    putStrLn "Enter a line:"
    input <- getLine
    let reversed = reverse input
    putStrLn ("Reversed: " ++ reversed)

