module HasperTypes
  ( HasperTask
      ( HasperTask,
        taskId,
        taskDate,
        taskText,
        taskIsCompleted
      ),
  )
where

import Control.Monad (MonadPlus (mzero))
import Data.Csv (FromRecord (parseRecord), ToField (toField), ToRecord (toRecord), record, (.!))
import Data.DateTime (DateTime, fromGregorian, toGregorian)

data HasperTask = HasperTask
  { taskId :: Int,
    taskDate :: DateTime,
    taskText :: String,
    taskIsCompleted :: Bool
  }
  deriving (Show)

instance FromRecord HasperTask where
  parseRecord v
    | length v == 8 =
      HasperTask
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

instance ToRecord HasperTask where
  toRecord (HasperTask id date text isCompleted) =
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
