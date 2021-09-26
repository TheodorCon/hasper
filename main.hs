module Main where

import System.Directory
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))
import qualified System.IO.Strict as S

getDefaultDirectory :: IO FilePath
getDefaultDirectory = (++ "/.hasper") <$> getHomeDirectory

getDefaultFile :: IO FilePath
getDefaultFile = (++ "/default.txt") <$> getDefaultDirectory

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r"] -> getDefaultFile >>= readFile >>= putStrLn
    _ -> do
      defaultDirectory <- getDefaultDirectory
      dirExists <- doesDirectoryExist defaultDirectory
      if dirExists
        then writeInFile (head args)
        else do
          createDirectory defaultDirectory
          writeInFile (head args)

writeInFile :: String -> IO ()
writeInFile task = do
  defaultFile <- getDefaultFile
  fileExists <- doesFileExist defaultFile
  if fileExists
    then do
      contents <- S.run . S.readFile $ defaultFile
      writeFile defaultFile (contents ++ "\n" ++ task)
    else writeFile defaultFile task