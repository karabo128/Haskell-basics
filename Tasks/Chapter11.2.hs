-- HC11T1: Greet the User
main :: IO ()
main = do
    putStrLn "What is your name?"
    let name = "Tester"  
    putStrLn ("Hello, " ++ name ++ "!")

-- HC11T2: Count Characters in a Line
main :: IO ()
main = do
    putStrLn "Enter a line:"
    let input = "Test input"  
    let lengthOfInput = length input
    putStrLn ("Number of characters: " ++ show lengthOfInput)

--HC11T3: Double a Number
main :: IO ()
main = do
    putStrLn "Enter a number:"
    let input = "5"     
    let number = read input :: Int
    putStrLn ("Double the number: " ++ show (number * 2))

-- HC11T4: Concatenate Two Lines
main :: IO ()
main = do
    putStrLn "Enter the first line:"
    let line1 = "Hello, "
    putStrLn "Enter the second line:"
    let line2 = "world!"
    putStrLn ("Concatenated result: " ++ line1 ++ line2)


-- HC11T5: Repeat Until "quit"

main :: IO ()
main = do
    
    let simulateRepeat "quit" = putStrLn "Goodbye!"
        simulateRepeat cmd = do
            putStrLn ("You entered: " ++ cmd)
            simulateRepeat "quit"  
    putStrLn "Enter a command:"
    let command = "hello"  
    simulateRepeat command

-- HC11T6: Uppercase Converter
import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "Enter a line:"
    let input = "hello world"
    let uppercased = map toUpper input
    putStrLn ("Uppercase: " ++ uppercased)
    
-- HC11T7: User Options
main :: IO ()
main = do
    putStrLn "Choose an option:"
    putStrLn "1. Greet"
    putStrLn "2. Exit"
    
    let choice = "1"
    case choice of
        "1" -> putStrLn "Hello, User!"
        "2" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid choice!"
        
-- HC11T8: Even or Odd Checker

main :: IO ()
main = do
    putStrLn "Enter a number:"
    
    let input = "7"
    let number = read input :: Int
    if even number
        then putStrLn "The number is even."
        else putStrLn "The number is odd."

-- HC11T9: Sum Two Numbers

main :: IO ()
main = do
    putStrLn "Enter the first number:"
    let num1 = "3"
    putStrLn "Enter the second number:"
    let num2 = "4"
    let sumOfNumbers = (read num1 :: Int) + (read num2 :: Int)
    putStrLn ("The sum is: " ++ show sumOfNumbers)


-- HC11T10: Reverse User Input
main :: IO ()
main = do
    putStrLn "Enter a line:"
    let input = "Haskell"
    let reversed = reverse input
    putStrLn ("Reversed: " ++ reversed)

