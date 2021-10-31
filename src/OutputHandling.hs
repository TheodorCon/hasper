module OutputHandling (printTasks) where

import Data.DateTime (formatDateTime)
import HasperTypes (HasperTask (HasperTask, taskDate, taskId, taskIsCompleted, taskText))

taskToString :: HasperTask -> String
taskToString task =
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

printTasks :: [HasperTask] -> IO ()
printTasks = putStrLn . unlines . map taskToString