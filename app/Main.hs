module Main (main) where

import System.Environment (getArgs)
import System.Directory

import Flisp

import Output
import Parse.S19

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "No file was given!"
  else
    let
      path = args !! 0 :: FilePath
    in do
      exists <- doesFileExist path
      if exists
      then do
        contents <- readFile path
        let memory = parse $ lines contents
        runFlisp $ flash memory
      else putStrLn $ path ++ " does NOT exist!"
