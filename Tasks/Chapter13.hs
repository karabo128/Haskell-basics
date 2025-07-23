-- HC13T1: List Files in Directory
import System.Directory (listDirectory)

main :: IO ()
main = do
    files <- listDirectory "."  -- List files in current directory
    mapM_ putStrLn files

-- HC13T2: Filter Files by Substring
import System.Directory (listDirectory)
import Data.List (isInfixOf)

filterFiles :: String -> IO [String]
filterFiles substring = do
    files <- listDirectory "."
    return $ filter (isInfixOf substring) files

main :: IO ()
main = do
    filteredFiles <- filterFiles "test"  -- Example: files containing "test"
    mapM_ putStrLn filteredFiles

-- HC13T3: Sort and Return Filtered Files
import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)

filterAndSortFiles :: String -> IO [String]
filterAndSortFiles substring = do
    files <- listDirectory "."
    let filtered = filter (isInfixOf substring) files
    return $ sort filtered

main :: IO ()
main = do
    sortedFiles <- filterAndSortFiles "test"  -- Example: files containing "test"
    mapM_ putStrLn sortedFiles

-- HC13T4: SumNonEmpty Module
module SumNonEmpty (sumNonEmpty) where

sumNonEmpty :: (Num a) => [a] -> a
sumNonEmpty [] = error "Empty list"
sumNonEmpty xs = sum xs

main :: IO ()
main = do
    print $ sumNonEmpty [1, 2, 3, 4]  -- 10
    -- Uncomment the following to see the error for an empty list:
    -- print $ sumNonEmpty []  -- Will throw error

-- HC13T5: Restrict Module Export List
module SumNonEmpty (sumNonEmpty) where

sumNonEmpty :: (Num a) => [a] -> a
sumNonEmpty [] = error "Empty list"
sumNonEmpty xs = sum xs

-- Helper function not exported:
-- sumHelper :: (Num a) => [a] -> a
-- sumHelper = sum

-- HC13T6: File Names to Map
import System.Directory (listDirectory)
import Data.Map (fromList)

fileNamesToMap :: IO (Data.Map.Map String Int)
fileNamesToMap = do
    files <- listDirectory "."
    return $ fromList $ zip files [1..]  -- Map filenames to unique IDs

main :: IO ()
main = do
    fileMap <- fileNamesToMap
    print fileMap

-- HC13T7: Use Custom Module in Main
import SumNonEmpty (sumNonEmpty)

main :: IO ()
main = do
    print $ sumNonEmpty [1, 2, 3, 4]  -- 10
    print $ sumNonEmpty [5, 5, 5]     -- 15

-- HC13T8: Qualified Imports for Name Conflicts
import qualified Data.List as L
import qualified Data.Map as M

main :: IO ()
main = do
    let list1 = [1, 2, 3]
    let list2 = [3, 4, 5]
    print $ L.intersect list1 list2  -- Intersection using Data.List
    let map1 = M.fromList [("a", 1), ("b", 2)]
    print $ M.lookup "a" map1       -- Lookup using Data.Map

-- HC13T9: Renaming Module Namespace
import qualified Data.List as L
import qualified System.Directory as SD

main :: IO ()
main = do
    files <- SD.listDirectory "."  -- Using System.Directory with alias SD
    let sortedFiles = L.sort files  -- Using Data.List with alias L
    mapM_ putStrLn sortedFiles

-- HC13T10: Multi-Module Main Function
import System.Directory (listDirectory)
import Data.List (sort, isInfixOf)

filterAndSortFiles :: String -> IO [String]
filterAndSortFiles substring = do
    files <- listDirectory "."
    let filtered = filter (isInfixOf substring) files
    return $ sort filtered

main :: IO ()
main = do
    sortedFiles <- filterAndSortFiles "log"  -- Example: files containing "log"
    mapM_ putStrLn sortedFiles
