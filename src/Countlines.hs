module Countlines
    -- ( countFunc
    -- )
    where

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List

-- countFunc = do
--      --arg <- getArgs
--      n <- countLines ["src/test.txt"]
--      when (n >= 0) $ putStrLn $ show n

countLines :: [String] -> IO Int
countLines [filename] = countComplexity filename
countLines [path, txt] = countLinesInDirectory path txt
countLines [] = do
    putStrLn "Usage:"
    putStrLn "CountLines file_path           : Count the lines of the file specified"
    putStrLn "CountLines directory_path txt  : Count the lines of the files whose names are suffixed with txt under the directory specified recursively"
    return (-1)
countLines _ = do
    putStrLn "Invalid parameters!"
    return (-1)


countComplexity :: String -> IO Int
countComplexity filename = do
    content <- readFile filename
    return $ length $ lines content

countLinesInDirectory :: String -> String -> IO Int
countLinesInDirectory path txt = do
    filelist <- getDirectoryContents path
        >>= filterM (\name -> return $ name /= ".." && name /= ".")
        >>= mapM (return . (path_++))
        >>= filterM (isFileType txt)
    mapM_ putStrLn filelist
    lineCounts <- mapM count filelist
    return $ sum lineCounts
    where
        isFileType :: String -> String -> IO Bool
        isFileType ext name = do
            b <- doesFileExist name
            if b
                then return $ isSuffixOf txt name
                else return True
        count name = do
            b <- doesFileExist name
            if b
                then countComplexity name
                else countLinesInDirectory name txt
        path_ = path ++ "/"

getFileName :: String -> IO [String]
getFileName fn = do
  names <- readFile fn
  return $ lines names
