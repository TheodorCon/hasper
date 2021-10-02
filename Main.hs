module Main where

import Control.Monad
import Data.Csv
import Data.DateTime
-- import Data.
import System.Directory
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))
import qualified System.IO.Strict as S

mockTask :: Task
mockTask =
  Task
    { taskId = -1,
      taskDate = fromGregorian 2021 1 1 0 0 0,
      taskText = "mock text",
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
    -- ["-tw"] -> encode mockTask >>= writeInDefault
    -- ["-tr"] -> (getDefaultFile >>= readFile . decode NoHeader) >>= putStrLn
    ["-r"] -> getDefaultFile >>= readFile >>= putStrLn
    _ -> do
      defaultDirectory <- getDefaultDirectory
      dirExists <- doesDirectoryExist defaultDirectory
      if dirExists
        then writeInDefault (head args)
        else do
          createDirectory defaultDirectory
          writeInDefault (head args)

reset :: IO ()
reset = do
  defaultFile <- getDefaultFile
  fileExists <- doesFileExist defaultFile
  when fileExists $ do
    writeFile defaultFile ""

writeInDefault :: String -> IO ()
writeInDefault task = do
  defaultFile <- getDefaultFile
  writeInFile defaultFile task

writeInFile :: FilePath -> String -> IO ()
writeInFile file task = do
  fileExists <- doesFileExist file
  if fileExists
    then do
      contents <- S.run . S.readFile $ file
      writeFile file (contents ++ "\n" ++ task)
    else writeFile file task

printTask :: Task -> String
printTask task =
  let id = taskId task
      date = taskDate task
      text = taskText task
      isCompleted = taskIsCompleted task
   in show id ++ "\t"
        ++ formatDateTime "%d.%m.%Y %H:%M" date
        ++ text
        ++ (if isCompleted then "Completed" else "Ongoing")

taskToCsv :: Task -> String
taskToCsv task =
  let id = taskId task
      (year, month, day, hour, minute, _) = (toGregorian . taskDate) task
      text = taskText task
      isCompleted = taskIsCompleted task
   in error "unimplemented"

stringToTask :: String -> Task
stringToTask string =
  let constituents = words string
   in undefined
