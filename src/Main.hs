module Main where

-- import Data.

import InputHandling
import Control.Concurrent
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Csv
import Data.DateTime
  ( DateTime,
    formatDateTime,
    fromGregorian,
    getCurrentTime,
    toGregorian,
  )
import Data.Functor
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import FileIO ( getDefaultFile, initializeFiles, resetDefault, getNextTaskId, writeInDefault )
import HasperTypes
  ( HasperTask
      ( HasperTask,
        taskDate,
        taskId,
        taskIsCompleted,
        taskText
      ),
  )
import System.IO
import qualified System.IO.Strict as S

mockTask :: HasperTask
mockTask =
  HasperTask
    { taskId = -1,
      taskDate = fromGregorian 2021 1 1 0 0 0,
      taskText = "mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text mock text ",
      taskIsCompleted = False
    }

main :: IO ()
main =
  initializeFiles
    *> (handleArgs =<< parseHasperArgs)

handleArgs :: HasperCommand -> IO ()
handleArgs (HasperNewTask text) = putStrLn "Task saved" *> makeTaskDefault text >>= writeInDefault . (BSLC.toStrict . encode . (: []))
handleArgs (HasperCompleteTask id) = do
  eitherTasks <- (getDefaultFile >>= BS.readFile) <&> (decode NoHeader . BSLC.fromStrict)
  case eitherTasks of
    Left errorMessageA -> putStrLn errorMessageA
    Right tasks -> case completeTask tasks id of
      Left errorMessageB -> putStrLn errorMessageB
      Right newTasks -> putStrLn "Task completed" *> resetDefault *> (writeInDefault . BSLC.toStrict . encode $ V.toList newTasks)
handleArgs HasperListAllTasks = getDefaultFile >>= BS.readFile >>= (putStrLn . csvToAllTaskString)
handleArgs HasperListProgTasks = getDefaultFile >>= BS.readFile >>= (putStrLn . csvToTaskString (not . taskIsCompleted))
handleArgs HasperListDoneTasks = getDefaultFile >>= BS.readFile >>= (putStrLn . csvToTaskString taskIsCompleted)
handleArgs HasperDevResetTasks = resetDefault <* putStrLn "Erased the default folder"

completeTask :: V.Vector HasperTask -> Int -> Either String (V.Vector HasperTask)
completeTask tasks id =
  let (before, after) = V.break (\t -> taskId t == id) tasks
      maybeTask = (V.!?) after 0
      maybeNewTask = (\t -> t {taskIsCompleted = True}) <$> maybeTask
   in case maybeNewTask of
        Nothing -> Left ("Could not find a task with id" ++ show id)
        Just newTask -> Right ((V.++) before (V.cons newTask (V.tail after)))

csvToAllTaskString :: BS.ByteString -> String
csvToAllTaskString = csvToTaskString (const True)

csvToTaskString :: (HasperTask -> Bool) -> BS.ByteString -> String
csvToTaskString pred = handleEither . decode NoHeader . BSLC.fromStrict
  where
    handleEither :: Either String (V.Vector HasperTask) -> String
    handleEither (Left errorMessage) = errorMessage
    handleEither (Right tasks) = "\n" ++ (V.foldl (\acc task -> acc ++ printTask task ++ "\n") "" . V.filter pred) tasks

printTask :: HasperTask -> String
printTask task =
  let id = taskId task
      date = taskDate task
      text = taskText task
      isCompleted = taskIsCompleted task
   in show id
        ++ "\t"
        ++ formatDateTime "%d.%m.%Y %H:%M" date
        ++ "\t"
        ++ (if isCompleted then "Done" else "Prog")
        ++ "\t"
        ++ text

makeTaskDefault :: String -> IO HasperTask
makeTaskDefault text = do
  defaultFile <- getDefaultFile
  makeTask defaultFile text

makeTask :: FilePath -> String -> IO HasperTask
makeTask file text = do
  currentTime <- getCurrentTime
  nextId <- getNextTaskId file
  return $
    HasperTask
      { taskId = nextId,
        taskDate = currentTime,
        taskText = text,
        taskIsCompleted = False
      }
