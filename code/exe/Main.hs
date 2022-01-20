module Main where

import System.Environment
import System.Directory
import Data.List
import Huffman

main :: IO()
main = do
    args <- getArgs
    print args
    validateArgs args
    pure ()

validateArgs :: [String] -> IO ()
validateArgs = chooseBehavior [
        (argumentCount (<2),   reportError "too few arguments.\n(needs source and destination path)."),
        (argumentCount (>2),   reportError "too many arguments.\n(needs source and destination path)."),
        (fileNotThere,         reportError "input file does not exist."),
        (equalPaths,           reportError "input and output file is the same, it would overwrite the source."),
        (hasExtension ".comp", doFileAction decodeFile),
        (always True,          doFileAction encodeFile)
    ]

doFileAction :: (FilePath -> FilePath -> IO ()) -> [FilePath] -> IO ()
doFileAction func [source, destination] = func source destination
doFileAction _ _ = undefined

-- Behavior table executor
chooseBehavior :: Monad m => [([a] -> m Bool, [a] -> m ())] -> [a] -> m ()
chooseBehavior [] _ = undefined
chooseBehavior [(_, call)] args = call args
chooseBehavior ((check, call):rest) args = do
    success <- check args
    if success
    then call args
    else chooseBehavior rest args

reportError :: String -> [String] -> IO ()
reportError message _ = putStrLn message

always :: Monad m => t -> [a] -> m t
always val _ = pure val

argumentCount :: (Int -> Bool) -> [String] -> IO Bool
argumentCount check list = pure $ check $ length list

fileNotThere :: [String] -> IO Bool
fileNotThere list = do
    result <- doesFileExist $ head list
    pure $ not result

hasExtension :: FilePath -> [String] -> IO Bool
hasExtension ext list = pure (isSuffixOf ext $ head list)

equalPaths :: [String] -> IO Bool
equalPaths [source, destination] = pure (source == destination)
equalPaths _ = undefined