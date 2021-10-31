module FileIO
  ( getDefaultDirectory,
    getDefaultFile,
    initializeFiles,
    resetDefault,
    writeInDefault,
    getNextTaskId,
    getTasks,
    addTask,
    completeTask,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLC
import Data.Csv (HasHeader (NoHeader), decode, encode)
import Data.DateTime (getCurrentTime)
import Data.Functor
import qualified Data.Vector as V
import HasperTypes (HasperTask (HasperTask, taskDate, taskId, taskIsCompleted, taskText))
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath (takeDirectory)

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

writeTaskToFile :: FilePath -> HasperTask -> IO ()
writeTaskToFile file task = writeInFile file $ (BSLC.toStrict . encode . (: [])) task

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

getTasks :: FilePath -> IO [HasperTask]
getTasks file = csvToTasks <$> BS.readFile file

csvToTasks :: BS.ByteString -> [HasperTask]
csvToTasks = handleEither . decode NoHeader . BSLC.fromStrict
  where
    handleEither (Left errorMessage) = error "The task file seems invalid"
    handleEither (Right tasks) = V.toList tasks

addTask :: FilePath -> String -> IO ()
addTask file text = createTask file text >>= writeTaskToFile file

createTask :: FilePath -> String -> IO HasperTask
createTask file text = do
  currentTime <- getCurrentTime
  nextId <- getNextTaskId file
  return $
    HasperTask
      { taskId = nextId,
        taskDate = currentTime,
        taskText = text,
        taskIsCompleted = False
      }

completeTask :: FilePath -> Int -> IO ()
completeTask file id = do
  eitherTasks <- (getDefaultFile >>= BS.readFile) <&> (decode NoHeader . BSLC.fromStrict)
  case eitherTasks of
    Left errorMessageA -> putStrLn errorMessageA
    Right tasks -> case changeAndReplace tasks id of
      Left errorMessageB -> putStrLn errorMessageB
      Right newTasks -> putStrLn "Task completed" *> resetDefault *> (writeInDefault . BSLC.toStrict . encode $ V.toList newTasks)
      where
        changeAndReplace :: V.Vector HasperTask -> Int -> Either String (V.Vector HasperTask)
        changeAndReplace tasks id =
          let (before, after) = V.break (\t -> taskId t == id) tasks
              maybeTask = (V.!?) after 0
              maybeNewTask = (\t -> t {taskIsCompleted = True}) <$> maybeTask
           in case maybeNewTask of
                Nothing -> Left ("Could not find a task with id" ++ show id)
                Just newTask -> Right ((V.++) before (V.cons newTask (V.tail after)))
