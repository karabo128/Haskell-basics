-- HC18T1: mapToLower Function with fmap
import Data.Char (toLower)

mapToLower :: [Char] -> [Char]
mapToLower = fmap toLower

main :: IO ()
main = do
    let str = "HELLO HASKELL"
    putStrLn $ "Lowercase: " ++ mapToLower str  

-- HC18T2: Functor Instance for Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node left right) = Node (fmap f left) (fmap f right)

main :: IO ()
main = do
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    print (fmap (*2) tree)  

-- HC18T3: incrementTreeValues Function
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

main :: IO ()
main = do
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    print (incrementTreeValues tree)  

-- HC18T4: mapToBits Function
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

main :: IO ()
main = do
    let boolList = [True, False, True, True]
    putStrLn $ "Mapped to bits: " ++ mapToBits boolList  

-- HC18T5: Functor Instance for Either
instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

main :: IO ()
main = do
    let result1 = fmap (+1) (Right 5)
    let result2 = fmap (+1) (Left "Error")
    print result1  
    print result2  

-- HC18T6: applyToMaybe Function
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

main :: IO ()
main = do
    let result1 = applyToMaybe (+1) (Just 5)
    let result2 = applyToMaybe (+1) Nothing
    print result1  
    print result2  

-- HC18T7: fmapTuple Function
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple f (x, y) = (x, f y)

main :: IO ()
main = do
    let tuple = ("Hello", 5)
    print (fmapTuple (*2) tuple)  

-- HC18T8: identityLawCheck Function
identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

main :: IO ()
main = do
    let result = identityLawCheck (Just 5)
    print result  

-- HC18T9: compositionLawCheck Function
compositionLawCheck :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
compositionLawCheck f g x = fmap (g . f) x == (fmap g . fmap f) x

main :: IO ()
main = do
    let result = compositionLawCheck (+1) (*2) (Just 5)
    print result  

-- HC18T10: nestedFmap Function
nestedFmap :: Functor f => (a -> b) -> f (f a) -> f (f b)
nestedFmap = fmap . fmap

main :: IO ()
main = do
    let nested = Just (Just 5)
    print (nestedFmap (*2) nested)  
