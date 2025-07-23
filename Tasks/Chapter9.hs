-- HC9T1: Define a Parametric Type Synonym
type Entity a = (a, String)  -- Example: (Person, "123 Street")

main :: IO ()
main = do
    let entity = (42, "John's Address") :: Entity Int
    print entity

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving Show

main :: IO ()
main = do
    let box1 = Has 5 :: Box Int
    let box2 = Empty :: Box Int
    print box1
    print box2

-- HC9T3: Function to Add Values in a Box
addN :: (Num a) => a -> Box a -> Box a
addN _ Empty = Empty
addN n (Has x) = Has (x + n)

main :: IO ()
main = do
    let boxWithValue = Has 10
    let result = addN 5 boxWithValue
    print result

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x

main :: IO ()
main = do
    let extractedValue = extract 0 (Has 15)
    print extractedValue

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a, radius :: Double } 
             | Rectangle { color :: a, width :: Double, height :: Double } 
             deriving Show

main :: IO ()
main = do
    let circle = Circle { color = "Red", radius = 5.0 }
    let rectangle = Rectangle { color = "Blue", width = 3.0, height = 4.0 }
    print circle
    print rectangle

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet { content :: String, likes :: Int, comments :: [Tweet] } deriving Show

main :: IO ()
main = do
    let tweet = Tweet { content = "Hello Haskell!", likes = 100, comments = [] }
    print tweet

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement tweet = likes tweet + sum (map engagement (comments tweet))

main :: IO ()
main = do
    let tweet = Tweet { content = "Hello Haskell!", likes = 100, comments = [] }
    let engagementResult = engagement tweet
    print engagementResult

-- HC9T8: Recursive Sequence Data Type
data Sequence a = EmptySeq | Node a (Sequence a) deriving Show

main :: IO ()
main = do
    let seq = Node 1 (Node 2 (Node 3 EmptySeq))
    print seq

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ EmptySeq = False
elemSeq x (Node y rest) = x == y || elemSeq x rest

main :: IO ()
main = do
    let seq = Node 1 (Node 2 (Node 3 EmptySeq))
    let isElem = elemSeq 2 seq
    print isElem

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyBST | NodeBST a (BST a) (BST a) deriving Show

main :: IO ()
main = do
    let bst = NodeBST 10 (NodeBST 5 EmptyBST EmptyBST) (NodeBST 15 EmptyBST EmptyBST)
    print bst
