module Main where

import Data.DateTime
  ( DateTime,
    formatDateTime,
    fromGregorian,
    getCurrentTime,
    toGregorian,
  )
import FileIO
  ( addTask,
    completeTask,
    getDefaultDirectory,
    getDefaultFile,
    getNextTaskId,
    getTasks,
    initializeFiles,
    resetDefault,
    writeInDefault,
  )
import HasperTypes
  ( HasperTask
      ( HasperTask,
        taskDate,
        taskId,
        taskIsCompleted,
        taskText
      ),
  )
import InputHandling ( HasperCommand(..), parseHasperArgs )
import OutputHandling (printTasks)

main :: IO ()
main =
  initializeFiles
    *> (handleArgs =<< parseHasperArgs)

handleArgs :: HasperCommand -> IO ()
handleArgs (HasperNewTask text) = do
  defaultFile <- getDefaultFile
  putStrLn "Task saved" *> addTask defaultFile text
handleArgs (HasperCompleteTask id) = do
  defaultFile <- getDefaultFile
  completeTask defaultFile id
handleArgs HasperListAllTasks = getDefaultFile >>= getTasks >>= printTasks
handleArgs HasperListProgTasks = getDefaultFile >>= getTasks >>= (printTasks . filter (not . taskIsCompleted))
handleArgs HasperListDoneTasks = getDefaultFile >>= getTasks >>= (printTasks . filter taskIsCompleted)
handleArgs HasperDevResetTasks = resetDefault <* putStrLn "Erased the default folder"
