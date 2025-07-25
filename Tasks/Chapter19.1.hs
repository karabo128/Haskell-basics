-- HC19T1: Applicative Instance for Pair
-- Define a Pair data type
data Pair a = Pair a a
  deriving (Show, Eq)

-- Define the Applicative instance for Pair
instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

main :: IO ()
main = do
    let pair1 = Pair (+1) (*2)
    let pair2 = Pair 3 4
    print (pair1 <*> pair2)  

-- HC19T2: addThreeApplicative Function
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative x y z = (+) <$> x <*> y <*> z

main :: IO ()
main = do
    let result = addThreeApplicative (Just 1) (Just 2) (Just 3)
    print result 

-- HC19T3: safeProduct for Maybe Int
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (\x acc -> (*) <$> x <*> acc) (Just 1)

main :: IO ()
main = do
    let nums = [Just 2, Just 3, Just 4]
    let result = safeProduct nums
    print result 
    let numsWithNothing = [Just 2, Nothing, Just 4]
    print (safeProduct numsWithNothing)  

-- HC19T4: liftAndMultiply with liftA2
liftAndMultiply :: Int -> Int -> Maybe Int
liftAndMultiply = liftA2 (*) (Just 3) (Just 4)

main :: IO ()
main = do
    print (liftAndMultiply 3 4)  

-- HC19T5: applyEffects with <*>
import Control.Monad (when)

applyEffects :: IO Int -> IO Int -> IO Int
applyEffects action1 action2 = do
    x <- action1
    y <- action2
    print x
    print y
    return (x + y)

main :: IO ()
main = do
    let action1 = return 5
    let action2 = return 10
    applyEffects action1 action2  

-- HC19T6: repeatEffect with forever
import Control.Monad (forever)

repeatEffect :: IO () -> IO ()
repeatEffect = forever

main :: IO ()
main = do
    let action = putStrLn "Repeating..."
    repeatEffect action  

-- HC19T7: conditionalPrint with when
import Control.Monad (when)

conditionalPrint :: Bool -> IO ()
conditionalPrint condition = when condition (putStrLn "Condition is true!")

main :: IO ()
main = do
    conditionalPrint True   
    conditionalPrint False  

-- HC19T8: discardSecond with <*
discardSecond :: IO a -> IO b -> IO a
discardSecond action1 action2 = action1 <* action2

main :: IO ()
main = do
    discardSecond (putStrLn "First Action") (putStrLn "Second Action") 

-- HC19T9: pureAndApply Demonstration
pureAndApply :: Maybe Int -> Maybe Int -> Maybe Int
pureAndApply x y = pure (+) <*> x <*> y

main :: IO ()
main = do
    let result = pureAndApply (Just 5) (Just 3)
    print result  

-- HC19T10: combineResults for Either
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = (<*>) <$> pure (+) <*> <*> 

main :: IO ()
main = do
    print (combineResults (Right 3) (Right 5))  
    print (combineResults (Right 3) (Left "Error"))  
