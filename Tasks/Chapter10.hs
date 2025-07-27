-- HC10T1: ShowSimple Type Class
class ShowSimple a where showSimple :: a -> String

data PaymentMethod = CreditCard String | Cash Double deriving Show

instance ShowSimple PaymentMethod where
    showSimple (CreditCard number) = "Credit Card: " ++ number
    showSimple (Cash amount) = "Cash: " ++ show amount

main :: IO ()
main = do
    let payment = CreditCard "1234-5678-9876-5432"
    print $ showSimple payment

-- HC10T2: Summable Type Class
class Summable a where  
  sumUp :: [a] -> a

instance Summable Int where 
  sumUp = sum

main :: IO ()
main = do
  print $ sumUp ([1, 2, 3, 4] :: [Int])

-- HC10T3: Comparable Type Class
class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable String where
    compareWith = compare

main :: IO ()
main = do
    print $ compareWith "hello" "world"

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving Show

instance Eq a => Eq (Box a) where
    Empty == Empty = True
    Has x == Has y = x == y
    _ == _ = False

main :: IO ()
main = do
    let box1 = Has 10 :: Box Int
    let box2 = Has 10 :: Box Int
    let box3 = Empty :: Box Int
    print $ box1 == box2
    print $ box1 == box3

-- HC10T5: ShowDetailed Type Class
class ShowDetailed a where
    showDetailed :: a -> String

data User = User { name :: String, age :: Int } deriving Show

instance ShowDetailed User where
    showDetailed (User name age) = "User: " ++ name ++ ", Age: " ++ show age

main :: IO ()
main = do
    let user = User "Alice" 30
    print $ showDetailed user

-- HC10T6: Mutual Recursion in Eq for Blockchain
data Blockchain = Block String [Blockchain] deriving Show

instance Eq Blockchain where
    (Block hash1 _) == (Block hash2 _) = hash1 == hash2

main :: IO ()
main = do
    let blockchain1 = Block "hash1" []
    let blockchain2 = Block "hash1" []
    let blockchain3 = Block "hash2" []
    print $ blockchain1 == blockchain2
    print $ blockchain1 == blockchain3

-- HC10T7: Convertible Type Class

data PaymentMethod = CreditCard String | Cash Int deriving (Show)

class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert (CreditCard _) = "Credit Card"
    convert (Cash _)       = "Cash"

main :: IO ()
main = do
    let paymentMethod = CreditCard "1234-5678-9876-5432"
    print (convert paymentMethod :: String)


-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq String where
    compareEquality x y = x == y

main :: IO ()
main = do
    print $ compareEquality "hello" "hello"
    print $ compareEquality "hello" "world"

-- HC10T9: MinMax Type Class
class Bounded a => MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

main :: IO ()
main = do
    print (minValue :: Int)
    print (maxValue :: Int)


-- HC10T10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable String where
    concatWith = (++)

main :: IO ()
main = do
    let str1 = "Hello"
    let str2 = "World"
    print $ concatWith str1 str2

