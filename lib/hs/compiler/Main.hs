module Main where

import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

import Syntax (thrift)

compile :: [FilePath] -> IO ()
compile files = do
    results <- mapM (parseFromFile thrift) files
    putStrLn (show results)
    return ()

main :: IO ()
main = do setNumCapabilities =<< getNumProcessors
          compile =<< getArgs
