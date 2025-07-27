-- HC11T1: WeAccept Instance for Box
class WeAccept a where
    accept :: a -> Bool

data Box a = Box a | EmptyBox deriving Show

instance WeAccept (Box a) where
    accept (Box _) = True
    accept EmptyBox = False

main :: IO ()
main = do
    let box1 = Box 42
    let box2 = EmptyBox
    print $ accept box1  
    print $ accept box2  

-- HC11T2: Fancy Function for WeAccept
class WeAccept a where
    accept :: a -> Bool
    fancyFunction :: a -> String

data Cardano = CardanoInstance deriving Show
data Cash = CashInstance deriving Show
data Country = CountryInstance deriving Show

instance WeAccept Cardano where
    accept _ = True
    fancyFunction _ = "Cardano Accepted!"

instance WeAccept Cash where
    accept _ = True
    fancyFunction _ = "Cash Accepted!"

instance WeAccept Country where
    accept _ = False
    fancyFunction _ = "Country Not Accepted!"

main :: IO ()
main = do
    print $ fancyFunction CardanoInstance
    print $ fancyFunction CashInstance
    print $ fancyFunction CountryInstance

-- HC11T3: Container Type Class for Box
class Container a where
    isEmpty :: a -> Bool
    contains :: Eq b => a -> b -> Bool
    replace :: a -> b -> a

data Box a = Box a | EmptyBox deriving Show

instance Container (Box a) where
    isEmpty EmptyBox = True
    isEmpty _ = False
    contains (Box x) y = x == y
    contains EmptyBox _ = False
    replace _ new = Box new

main :: IO ()
main = do
    let box1 = Box 42
    let emptyBox = EmptyBox
    print $ isEmpty box1   
    print $ contains box1 42 
    print $ replace box1 100 
    print $ isEmpty emptyBox 

-- HC11T4: Container Instance for Present
class Container a where
    isEmpty :: a -> Bool
    contains :: Eq b => a -> b -> Bool
    replace :: a -> b -> a

data Present a = Present a | NoPresent deriving Show

instance Container (Present a) where
    isEmpty NoPresent = True
    isEmpty _ = False
    contains (Present x) y = x == y
    contains NoPresent _ = False
    replace _ new = Present new

main :: IO ()
main = do
    let present1 = Present "Toy"
    let noPresent = NoPresent
    print $ isEmpty present1    
    print $ contains present1 "Toy"
    print $ replace present1 "Gift"
    print $ isEmpty noPresent 

-- HC11T5: guessWhat'sInside Function for Containers
class Container a where
    isEmpty :: a -> Bool
    contains :: Eq b => a -> b -> Bool

guessWhat'sInside :: (Container a, Eq b) => a -> b -> String
guessWhat'sInside container item
    | isEmpty container = "The container is empty."
    | contains container item = "Item found!"
    | otherwise = "Item not found."

data Box a = Box a | EmptyBox deriving Show

instance Container (Box a) where
    isEmpty EmptyBox = True
    isEmpty _ = False
    contains (Box x) y = x == y
    contains EmptyBox _ = False

main :: IO ()
main = do
    let box1 = Box "Toy"
    let emptyBox = EmptyBox
    print $ guessWhat'sInside box1 "Toy"   
    print $ guessWhat'sInside box1 "Gift"  
    print $ guessWhat'sInside emptyBox "Toy" 


-- HC11T5: guessWhat'sInside Function for Containers
class Container a where
    isEmpty :: a -> Bool
    contains :: Eq b => a -> b -> Bool

guessWhat'sInside :: (Container a, Eq b) => a -> b -> String
guessWhat'sInside container item
    | isEmpty container = "The container is empty."
    | contains container item = "Item found!"
    | otherwise = "Item not found."

data Box a = Box a | EmptyBox deriving Show

instance Container (Box a) where
    isEmpty EmptyBox = True
    isEmpty _ = False
    contains (Box x) y = x == y
    contains EmptyBox _ = False

main :: IO ()
main = do
    let box1 = Box "Toy"
    let emptyBox = EmptyBox
    print $ guessWhat'sInside box1 "Toy"   
    print $ guessWhat'sInside box1 "Gift"  
    print $ guessWhat'sInside emptyBox "Toy" 

-- HC11T6: AdvancedEq for Blockchain
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

data Blockchain = Block String | EmptyBlock deriving Show

instance Eq Blockchain where
    (Block x) == (Block y) = x == y
    EmptyBlock == EmptyBlock = True
    _ == _ = False

instance AdvancedEq Blockchain where
    compareEquality x y = x == y

main :: IO ()
main = do
    let block1 = Block "Genesis"
    let block2 = Block "Genesis"
    let block3 = Block "Transaction"
    print $ compareEquality block1 block2  
    print $ compareEquality block1 block3  


-- HC11T7: Ord Instance for Box
data Box a = Box a | EmptyBox deriving Show

instance Ord (Box Int) where
    (Box x) `compare` (Box y) = x `compare` y
    EmptyBox `compare` _ = LT
    _ `compare` EmptyBox = GT

main :: IO ()
main = do
    let box1 = Box 10
    let box2 = Box 20
    let emptyBox = EmptyBox
    print $ box1 `compare` box2   
    print $ box1 `compare` emptyBox 
    print $ emptyBox `compare` emptyBox 

-- HC11T8: Deriving Eq and Ord for PaymentMethod
data PaymentMethod = Cash | Card | PayPal deriving (Eq, Ord, Show)

main :: IO ()
main = do
    let payment1 = Card
    let payment2 = PayPal
    print $ payment1 == payment2  
    print $ payment1 `compare` payment2  

-- HC11T9: Data Type Length with Units
data Length = Meters Int | Km Int deriving (Eq, Show)

instance Eq Length where
    (Meters x) == (Meters y) = x == y
    (Km x) == (Km y) = x == y
    (Meters x) == (Km y) = x * 1000 == y
    (Km x) == (Meters y) = x == y `div` 1000

main :: IO ()
main = do
    print $ Meters 1000 == Km 1  
    print $ Meters 500 == Km 1   

-- HC11T10: sortContainers Function
data Container = BoxInt Int | EmptyContainer deriving (Show, Ord)

main :: IO ()
main = do
    let containers = [BoxInt 5, BoxInt 3, BoxInt 8, EmptyContainer]
    print $ sort containers
