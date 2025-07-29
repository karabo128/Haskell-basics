-- HC11T1: Applicative Instance for Wrapper
-- Define a custom data type Wrapper a
data Wrapper a = Wrapper a
  deriving (Show, Eq)

-- Define the Applicative instance for Wrapper
instance Applicative Wrapper where
  pure x = Wrapper x
  Wrapper f <*> Wrapper x = Wrapper (f x)

main :: IO ()
main = do
    let wrapper1 = Wrapper (+1)
    let wrapper2 = Wrapper 5
    print (wrapper1 <*> wrapper2) 

-- HC11T2: sumThreeApplicative for Either String Int
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative x y z = (+) <$> x <*> y <*> z

main :: IO ()
main = do
    let result = sumThreeApplicative (Right 1) (Right 2) (Right 3)
    print result 
    let errorResult = sumThreeApplicative (Right 1) (Left "Error") (Right 3)
    print errorResult 

-- HC12T1: whenApplicative Function
import Control.Monad (when)

whenApplicative :: Applicative f => Bool -> f () -> f ()
whenApplicative condition action = if condition then action else pure ()

main :: IO ()
main = do
    whenApplicative True (putStrLn "Condition is true!")  
    whenApplicative False (putStrLn "This won't print.")  

-- HC12T2: replicateEffect with replicateM
import Control.Monad (replicateM)

replicateEffect :: IO a -> Int -> IO [a]
replicateEffect action n = replicateM n action

main :: IO ()
main = do
    replicateEffect (putStrLn "Action executed") 3  

-- HC13T1: sequenceEffects for Applicative List
sequenceEffects :: [IO a] -> IO [a]
sequenceEffects = sequence

main :: IO ()
main = do
    let actions = [putStrLn "Action 1", putStrLn "Action 2", putStrLn "Action 3"]
    sequenceEffects actions  

-- HC14T1: applyWithEffects and <*>
applyWithEffects :: IO Int -> IO Int -> IO Int
applyWithEffects action1 action2 = (+) <$> action1 <*> action2

main :: IO ()
main = do
    let action1 = return 3
    let action2 = return 5
    result <- applyWithEffects action1 action2
    print result 

-- HC15T1: simulateMaybeEffect for Multiple Maybe
simulateMaybeEffect :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
simulateMaybeEffect x y z = (+) <$> x <*> y <*> z

main :: IO ()
main = do
    let result = simulateMaybeEffect (Just 1) (Just 2) (Just 3)
    print result 
    let resultWithNothing = simulateMaybeEffect (Just 1) Nothing (Just 3)
    print resultWithNothing  

-- HC16T1: combineEitherResults with Multiple Either
combineEitherResults :: Either String Int -> Either String Int -> Either String Int -> Either String Int
combineEitherResults x y z = (+) <$> x <*> y <*> z

main :: IO ()
main = do
    let result = combineEitherResults (Right 1) (Right 2) (Right 3)
    print result  
    let resultWithError = combineEitherResults (Right 1) (Left "Error") (Right 3)
    print resultWithError 

-- HC17T1: sequenceApplicative for Maybe List
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

main :: IO ()
main = do
    let result = sequenceApplicative [Just 1, Just 2, Just 3]
    print result  
    let resultWithNothing = sequenceApplicative [Just 1, Nothing, Just 3]
    print resultWithNothing  

-- HC18T1: replicateForever with forever
import Control.Monad (forever)

replicateForever :: IO () -> IO ()
replicateForever = forever

main :: IO ()
main = do
    let action = putStrLn "Repeating forever..."
    replicateForever action  

