-- HC8T1: Type Synonyms and Basic Function
module HC8T1 where

type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val = "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val

main :: IO ()
main = do
  putStrLn "HC8T1:"
  print (generateTx "AliceAddr" "BobAddr" 100)

-- HC8T2: New Types and Data Constructors
module HC8T2 where

data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 42) Cash

main :: IO ()
main = do
  putStrLn "HC8T2:"
  print bob

-- HC8T3: Algebraic Data Types and Functions
module HC8T3 where

data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main :: IO ()
main = do
  putStrLn "HC8T3:"
  let c = Circle 5
  let r = Rectangle 10 5
  print (area c)
  print (area r)

-- HC8T4: Record Syntax for Employee
module HC8T4 where

data Employee = Employee { name :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

main :: IO ()
main = do
  putStrLn "HC8T4:"
  print richard

-- HC8T5: Record Syntax for Person
module HC8T5 where

data Person = Person { name :: String, age :: Int, isEmployed :: Bool } deriving Show

person1 :: Person
person1 = Person "Alice" 30 True

person2 :: Person
person2 = Person "Eve" 25 False

main :: IO ()
main = do
  putStrLn "HC8T5:"
  print person1
  print person2

-- HC8T6: Record Syntax for Shape Variants
module HC8T6 where

data Shape
  = Circle { center :: (Float, Float), color :: String, radius :: Float }
  | Rectangle { width :: Float, height :: Float, color :: String }
  deriving Show

circleShape :: Shape
circleShape = Circle (0, 0) "Red" 10.0

rectangleShape :: Shape
rectangleShape = Rectangle 20.0 15.0 "Blue"

main :: IO ()
main = do
  putStrLn "HC8T6:"
  print circleShape
  print rectangleShape

-- HC8T7: Data Types and Describing Animals
module HC8T7 where

data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Whiskers"

main :: IO ()
main = do
  putStrLn "HC8T7:"
  print (describeAnimal dog1)
  print (describeAnimal cat1)

-- HC8T8: Type Synonyms and Greeting Function
module HC8T8 where

type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, " ++ n ++ "! You are " ++ show a ++ " years old."

main :: IO ()
main = do
  putStrLn "HC8T8:"
  print (greet "Charlie" 45)

-- HC8T9: Record Type and Transaction Function
module HC8T9 where

type Address = String
type Value = Int

data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t a = transactionId tx
  where
    tx = Transaction f t a ("tx_" ++ take 5 f ++ "_to_" ++ take 5 t)

main :: IO ()
main = do
  putStrLn "HC8T9:"
  print (createTransaction "AliceWallet" "BobWallet" 250)

-- HC8T10: Deriving Show for Book
module HC8T10 where

data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving Show

book1 :: Book
book1 = Book "Learn You a Haskell" "Miran Lipovača" 2011

main :: IO ()
main = do
  putStrLn "HC8T10:"
  print book1
