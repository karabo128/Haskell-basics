-- HC17T1: Severity Data Type and Semigroup Instance
data Severity = Low | Medium | High | Critical
  deriving (Show, Eq)

instance Semigroup Severity where
  Low <> Low = Low
  Low <> Medium = Medium
  Low <> High = High
  Low <> Critical = Critical
  Medium <> Low = Medium
  Medium <> Medium = Medium
  Medium <> High = High
  Medium <> Critical = Critical
  High <> Low = High
  High <> Medium = High
  High <> High = High
  High <> Critical = Critical
  Critical <> Low = Critical
  Critical <> Medium = Critical
  Critical <> High = Critical
  Critical <> Critical = Critical

main :: IO ()
main = do
    let severity1 = Medium
    let severity2 = High
    print (severity1 <> severity2)  

    -- HC17T2: Min and Max Newtypes with Semigroup
newtype Min a = Min { getMin :: a } deriving (Show, Eq)
newtype Max a = Max { getMax :: a } deriving (Show, Eq)

instance Ord a => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)

instance Ord a => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

main :: IO ()
main = do
    let minVal = Min 10
    let maxVal = Max 20
    print (minVal <> Min 5)  
    print (maxVal <> Max 25) 

    -- HC17T3: Monoid Instance for Severity
instance Monoid Severity where
  mempty = Low
  mappend = (<>)

main :: IO ()
main = do
    let severity1 = Medium
    let severity2 = Critical
    let combined = severity1 <> severity2
    print combined  
    print (mempty :: Severity)  

-- HC17T4: Monoid Instance for Sum Newtype
newtype Sum a = Sum { getSum :: a } deriving (Show, Eq)

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

main :: IO ()
main = do
    let sum1 = Sum 10
    let sum2 = Sum 20
    let combined = sum1 <> sum2
    print (getSum combined)  
    print (getSum (mempty :: Sum Int))  
    
    -- HC17T5: combineLists Function
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

main :: IO ()
main = do
    let list1 = [1, 2, 3]
    let list2 = [4, 5, 6]
    print (combineLists list1 list2)  ]

-- HC17T6: maxSeverity Function
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

main :: IO ()
main = do
    let severities = [Low, High, Medium, Critical]
    print (maxSeverity severities)  

-- HC17T7: multiplyProducts Function
newtype Product a = Product { getProduct :: a } deriving (Show, Eq)

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 1

multiplyProducts :: [Product Int] -> Product Int
multiplyProducts = mconcat

main :: IO ()
main = do
    let products = [Product 2, Product 3, Product 4]
    print (getProduct (multiplyProducts products))  


-- HC17T8: foldWithSemigroup Function
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr (<>) mempty

main :: IO ()
main = do
    let numbers = [1, 2, 3, 4]
    print (foldWithSemigroup numbers) 

-- HC17T9: Config Data Type and Semigroup Instance
data Config = Config { loggingLevel :: Int, timeout :: Int, retries :: Int }
  deriving (Show, Eq)

instance Semigroup Config where
  (Config l1 t1 r1) <> (Config l2 t2 r2) =
    Config (max l1 l2) (min t1 t2) (max r1 r2)

main :: IO ()
main = do
    let config1 = Config 3 30 2
    let config2 = Config 5 20 3
    print (config1 <> config2)  

-- HC17T10: Monoid Instance for Config
instance Monoid Config where
  mempty = Config 0 maxBound 0
  mappend = (<>)

main :: IO ()
main = do
    let config1 = Config 3 30 2
    let config2 = Config 5 20 3
    print (config1 <> config2)  
    print (mempty :: Config)  


    



