module Main where

import Parser
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline
import System.IO
import System.Environment

pp :: [Expr] -> IO ()
pp [] = putStrLn "end"
pp (x:xs) = do
    putStrLn $ show $ x
    pp xs

process :: String -> IO ()
process src = do
    let res = parseTop src
    case res of
        Left err -> print err
        Right ex -> do
            putStrLn $ show $ length ex
            pp ex

-- input haskeline
repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> (liftIO $ process input) >> loop

processFile :: String -> IO ()
processFile f = readFile f >>= process

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [a] -> processFile a
