module FileIO (getDefaultDirectory, getDefaultFile, initializeFiles, resetDefault, writeInDefault, getNextTaskId) where

import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ( takeDirectory )
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.ByteString.Lazy as BSLC
import qualified Data.Vector as V
import HasperTypes (HasperTask)
import Data.Csv (decode, HasHeader (NoHeader))

getDefaultDirectory :: IO FilePath
getDefaultDirectory = (++ "/.hasper") <$> getHomeDirectory

getDefaultFile :: IO FilePath
getDefaultFile = (++ "/default.csv") <$> getDefaultDirectory

initializeFiles :: IO ()
initializeFiles = do
  defaultFile <- getDefaultFile
  fileExists <- doesFileExist defaultFile
  if fileExists
    then return ()
    else do
      createDirectoryIfMissing True $ takeDirectory defaultFile
      writeFile defaultFile ""
      
resetDefault :: IO ()
resetDefault = getDefaultFile >>= resetFile

resetFile :: FilePath -> IO ()
resetFile file = do
  fileExists <- doesFileExist file
  when fileExists $ do
    writeFile file ""

writeInDefault :: BS.ByteString -> IO ()
writeInDefault task = do
  defaultFile <- getDefaultFile
  writeInFile defaultFile task

writeInFile :: FilePath -> BS.ByteString -> IO ()
writeInFile file task = do
  fileExists <- doesFileExist file
  if fileExists
    then do
      contents <- BS.readFile file
      BS.writeFile file (contents <> task)
    else BS.writeFile file task
    
getNextDefaultTaskId :: IO Int
getNextDefaultTaskId = do
  defaultFile <- getDefaultFile
  getNextTaskId defaultFile

getNextTaskId :: FilePath -> IO Int
getNextTaskId file = do
  fileExists <- doesFileExist file
  if fileExists
    then do
      contents <- BS.readFile file
      case decode NoHeader (BSLC.fromStrict contents) :: Either String (V.Vector HasperTask) of
        Left errorMessage -> return (-1)
        Right taskVector -> return $ V.length taskVector
    else return (-1)