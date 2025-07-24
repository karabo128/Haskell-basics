-- HC20T1: safeDivide with Maybe Monad
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

main :: IO ()
main = do
    print (safeDivide 10 2)  
    print (safeDivide 10 0)  

-- HC20T2: sequenceMaybe for List of Maybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

main :: IO ()
main = do
    print (sequenceMaybe [Just 1, Just 2, Just 3])  
    print (sequenceMaybe [Just 1, Nothing, Just 3])  

-- HC20T3: Writer Monad Logging Calculator
import Control.Monad.Writer

type Log = [String]
type Calculator a = Writer Log a

add :: Int -> Int -> Calculator Int
add x y = tell ["Adding " ++ show x ++ " and " ++ show y] >> return (x + y)

subtract' :: Int -> Int -> Calculator Int
subtract' x y = tell ["Subtracting " ++ show y ++ " from " ++ show x] >> return (x - y)

main :: IO ()
main = do
    let result = execWriter (add 5 3 >> subtract' 8 4)
    print result  
-- HC20T4: countChars with State Monad
import Control.Monad.State

countChars :: Char -> String -> State Int Int
countChars char str = do
    let count = length (filter (== char) str)
    modify (+ count)
    return count

main :: IO ()
main = do
    let (result, finalState) = runState (countChars 'a' "banana") 0
    print result  
    print finalState 
-- HC20T5: Reader Monad for Configurable Greeting
import Control.Monad.Reader

data Config = Config { greeting :: String, name :: String }

greet :: Reader Config String
greet = do
    config <- ask
    return (greeting config ++ ", " ++ name config)

main :: IO ()
main = do
    let config = Config "Hello" "Alice"
    print (runReader greet config)  

-- HC20T6: doubleMonad Combining Maybe and List
doubleMonad :: Maybe Int -> [Int] -> Maybe [Int]
doubleMonad Nothing _ = Nothing
doubleMonad (Just x) xs = Just (map (* x) xs)

main :: IO ()
main = do
    print (doubleMonad (Just 2) [1, 2, 3])  
    print (doubleMonad Nothing [1, 2, 3])  

-- HC20T7: findFirst with Either Monad
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "Not Found"
findFirst p (x:xs)
  | p x       = Right x
  | otherwise = findFirst p xs

main :: IO ()
main = do
    print (findFirst even [1, 3, 5, 4])  
    print (findFirst even [1, 3, 5])    
-- HC20T8: Parser Monad for Simple Expressions
import Control.Monad.State

type Parser a = State String (Maybe a)

parseDigit :: Parser Int
parseDigit = do
    s <- get
    case s of
        (c:cs) | c `elem` ['0'..'9'] -> put cs >> return (read [c])
        _ -> return Nothing

parseExpr :: Parser Int
parseExpr = do
    x <- parseDigit
    return x

main :: IO ()
main = do
    let (result, _) = runState parseExpr "123abc"
    print result 

-- HC20T9: replicateMonad with Identity Monad
import Control.Monad.Identity

replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = return (replicate n x)

main :: IO ()
main = do
    print (runIdentity (replicateMonad 3 "hello"))  

-- HC20T10: Nested StateT and MaybeT Transformer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

type MyStateT = StateT Int (MaybeT IO)

nestedStateT :: MyStateT String
nestedStateT = do
    modify (+ 1)
    x <- lift (MaybeT (return (Just "Success")))
    return x

main :: IO ()
main = do
    result <- runMaybeT (evalStateT nestedStateT 0)
    print result  "
