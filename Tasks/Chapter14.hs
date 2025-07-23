-- HC14T1: Initialize a Cabal Project
-- Steps to initialize a Cabal project:
-- 1. Run `cabal init` in the terminal.
-- 2. Add the following in the `Main.hs` file:

main :: IO ()
main = putStrLn "Hello, Cabal!"

-- HC14T2: Add Dependency and Print Random Number
-- Add the `random` package to your .cabal file under build-depends:
-- build-depends:       base >=4.7 && <5, random

import System.Random (randomRIO)

main :: IO ()
main = do
    randomNumber <- randomRIO (1, 100) :: IO Int
    print randomNumber

-- HC14T3: NumericUnderscores Extension
{-# LANGUAGE NumericUnderscores #-}

main :: IO ()
main = do
    let largeNumber = 1_000_000_000
    print largeNumber

-- HC14T4: TypeApplications Extension
{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    let number = read @Int "1234" :: Int
    print number

-- HC14T5: Custom Data Type and Pattern Matching with @
data Result a = Success a | Failure String

main :: IO ()
main = do
    let result1 = Success 42
    let result2 = Failure "Error occurred"
    
    case result1 of
        Success val -> putStrLn $ "Success with value: " ++ show val
        Failure msg -> putStrLn $ "Failure: " ++ msg
    
    case result2 of
        Success val -> putStrLn $ "Success with value: " ++ show val
        Failure msg -> putStrLn $ "Failure: " ++ msg

-- HC14T6: Project Structure: src and app
-- Steps to create a project structure:
-- 1. Run `cabal init` and choose `app` and `lib` components.
-- 2. Create the structure like so:
--    myproject/
--      ├── app/
--      │   └── Main.hs
--      └── src/
--          └── MyModule.hs

-- Main.hs
module Main where
import MyModule (greet)

main :: IO ()
main = greet

-- MyModule.hs
module MyModule where

greet :: IO ()
greet = putStrLn "Hello from MyModule!"


-- HC14T7: Library Component in Cabal
-- Add a library component in your `.cabal` file:
-- library
--   exposed-modules:     MyLibrary
--   other-modules:       Paths_myproject

-- Then, in the `src/MyLibrary.hs`:
module MyLibrary where

sayHello :: IO ()
sayHello = putStrLn "Hello from the library!"

-- HC14T7: Library Component in Cabal
-- Add a library component in your `.cabal` file:
-- library
--   exposed-modules:     MyLibrary
--   other-modules:       Paths_myproject

-- Then, in the `src/MyLibrary.hs`:
module MyLibrary where

sayHello :: IO ()
sayHello = putStrLn "Hello from the library!"

-- HC14T8: Character Frequency Function
import Data.List (group, sort)

counts :: String -> [(Char, Int)]
counts str = map (\xs -> (head xs, length xs)) . group . sort $ str

main :: IO ()
main = do
    let result = counts "abbccc"
    print result  

-- HC14T9: PartialTypeSignatures Extension
{-# LANGUAGE PartialTypeSignatures #-}

add :: _ => Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
    print (add 5 10)  -- 15

-- HC14T10: Cabal Test Suite
-- In the `.cabal` file, add a test suite component:
-- test-suite myproject-test
--   type: exitcode-stdio-1.0
--   main-is: Test.hs
--   build-depends:       base >=4.7 && <5, myproject, hspec

-- Test.hs
import Test.Hspec
import MyModule (counts)

main :: IO ()
main = hspec $ do
    describe "counts function" $ do
        it "counts character frequencies" $
            counts "abbccc" `shouldBe` [('a',1), ('b',2), ('c',3)]
