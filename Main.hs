module Main where

import Control.Monad
-- import Data.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Csv
import Data.DateTime
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import System.Directory
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))
import qualified System.IO.Strict as S

mockTask :: Task
mockTask =
  Task
    { taskId = -1,
      taskDate = fromGregorian 2021 1 1 0 0 0,
      taskText = "mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text ",
      taskIsCompleted = False
    }

data Task = Task
  { taskId :: Int,
    taskDate :: DateTime,
    taskText :: String,
    taskIsCompleted :: Bool
  }
  deriving (Show)

instance FromRecord Task where
  parseRecord v
    | length v == 8 =
      Task
        <$> v .! 0
        <*> ( fromGregorian
                <$> v .! 1
                <*> v .! 2
                <*> v .! 3
                <*> v .! 4
                <*> v .! 5
                <*> return 0
            )
        <*> v .! 6
        <*> ((/= (0 :: Int)) <$> v .! 7)
    | otherwise = mzero

instance ToRecord Task where
  toRecord (Task id date text isCompleted) =
    let (year, month, day, hour, minute, _) = toGregorian date
     in record
          [ toField id,
            toField year,
            toField month,
            toField day,
            toField hour,
            toField minute,
            toField text,
            toField ((if isCompleted then 1 else 0) :: Int)
          ]

getDefaultDirectory :: IO FilePath
getDefaultDirectory = (++ "/.hasper") <$> getHomeDirectory

getDefaultFile :: IO FilePath
getDefaultFile = (++ "/default.csv") <$> getDefaultDirectory

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-tw"] -> writeInDefault . BSLC.toStrict . encode $ [mockTask]
    ["-tr"] -> getDefaultFile >>= readFile >>= (putStrLn . handleEither . (decode NoHeader . BSLC.pack))
      where
        handleEither :: Either String (V.Vector Task) -> String
        handleEither (Left errorMessage) = errorMessage
        handleEither (Right tasks) = V.foldl (\acc task -> acc ++ printTask task ++ "\n") "" tasks
    ["-r"] -> getDefaultFile >>= readFile >>= putStrLn
    ["-n", text] -> makeTaskDefault text >>= writeInDefault . (BSLC.toStrict . encode . (: []))
    _ -> putStrLn "Hasper is a cute little TODO tool made in Haskell"

reset :: IO ()
reset = do
  defaultFile <- getDefaultFile
  fileExists <- doesFileExist defaultFile
  when fileExists $ do
    writeFile defaultFile ""

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

printTask :: Task -> String
printTask task =
  let id = taskId task
      date = taskDate task
      text = taskText task
      isCompleted = taskIsCompleted task
   in show id
        ++ "\t"
        ++ formatDateTime "%d.%m.%Y %H:%M" date
        ++ "\t"
        ++ (if isCompleted then "Completed" else "Ongoing")
        ++ "\t"
        ++ text

stringToTask :: String -> Task
stringToTask string =
  let constituents = words string
   in undefined

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
      case decode NoHeader (BSLC.fromStrict contents) :: Either String (V.Vector Task) of
        Left errorMessage -> return (-1)
        Right taskVector -> return $ V.length taskVector
    else return (-1)

makeTaskDefault :: String -> IO Task
makeTaskDefault text = do
  defaultFile <- getDefaultFile
  makeTask defaultFile text

makeTask :: FilePath -> String -> IO Task
makeTask file text = do
  currentTime <- getCurrentTime
  nextId <- getNextTaskId file
  return $
    Task
      { taskId = nextId,
        taskDate = currentTime,
        taskText = text,
        taskIsCompleted = False
      }
