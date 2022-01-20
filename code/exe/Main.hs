module Main where

import System.Environment
--import System.Directory
--import Huffman
chooseBehavior :: Monad m => [([a] -> m Bool, [a] -> m ())] -> [a] -> m ()
chooseBehavior [] _ = undefined
chooseBehavior [(_, call)] args = call args
chooseBehavior ((check, call):rest) args = do
    success <- check args
    if success
    then call args
    else chooseBehavior rest args

constCheck :: Monad m => Bool -> [a] -> m Bool
constCheck val _ = pure val

validateArgs :: [String] -> IO ()
validateArgs = chooseBehavior [
        ((\list -> pure (length list < 2)), (\_ -> putStrLn "too few arguments\n(needs source and destination path)")),
        ((\list -> pure (length list > 2)), (\_ -> putStrLn "too many arguments\n(needs source and destination path)")),
--        ((\list -> doesFileExist (head list)), (\_ -> putStrLn "input file does not exist"))
        (constCheck True, program)
    ]

main :: IO()
main = do
    args <- getArgs
    print args
    validateArgs args
    pure ()

program :: [String] -> IO ()
program args = do 
    putStrLn ("program: " ++ show args)
    pure ()

compressFile :: [String] -> IO ()
compressFile args = do 
    putStrLn ("compress: " ++ show args)
    pure ()

decompressFile :: [String] -> IO ()
decompressFile args = do 
    putStrLn ("decompress: " ++ show args)
    pure ()