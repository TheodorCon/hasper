module Main where

import GHC.IO.Handle.FD (openFile)
import System.Directory
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))

getDefaultDirectory :: IO FilePath
getDefaultDirectory = (++ "/.hasper") <$> getHomeDirectory

getDefaultFile :: IO FilePath
getDefaultFile = (++ "/default.txt") <$> getDefaultDirectory

main :: IO ()
main = do
  args <- getArgs
  defaultDirectory <- getDefaultDirectory
  dirExists <- doesDirectoryExist defaultDirectory
  if dirExists
    then putStrLn $ "saved " ++ (args !! 0)
    else do
      createDirectory defaultDirectory
      putStrLn $ "saved " ++ (args !! 0)

-- writeInFile :: IO ()
-- writeInFile = do
--   defaultFile <- getDefaultFile
--   openFile defaultFile ReadMode $
--     \file -> do undefined