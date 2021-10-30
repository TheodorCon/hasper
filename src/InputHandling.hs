module InputHandling
  ( HasperCommand
      ( HasperNewTask,
        HasperCompleteTask,
        HasperListAllTasks,
        HasperListProgTasks,
        HasperListDoneTasks,
        HasperDevResetTasks
      ),
    parseHasperArgs,
  )
where

import Options.Applicative
import qualified Options.Applicative as OPT

data HasperCommand
  = HasperNewTask String
  | HasperCompleteTask Int
  | HasperListAllTasks
  | HasperListProgTasks
  | HasperListDoneTasks
  | HasperDevResetTasks

parseHasperArgs :: IO HasperCommand
parseHasperArgs = execParser infoHasperParser
  where
    infoHasperParser =
      info
        (hasperCommandParser <**> helper)
        ( fullDesc
            <> header "Hasper is a cute little TODO CLI application"
        )

hasperCommandParser :: OPT.Parser HasperCommand
hasperCommandParser =
  newTaskParser
    <|> completeTaskParser
    <|> listAllTaskParser
    <|> listProgTaskParser
    <|> listDoneTaskParser
    <|> devResetParser

newTaskParser :: OPT.Parser HasperCommand
newTaskParser =
  HasperNewTask
    <$> strOption
      ( long "new"
          <> short 'n'
          <> metavar "TASK_TEXT"
          <> help "The text describing your task"
      )

completeTaskParser :: OPT.Parser HasperCommand
completeTaskParser =
  HasperCompleteTask
    <$> option
      auto
      ( long "complete"
          <> short 'c'
          <> metavar "TASK_ID"
          <> help "The ID of the task you wish to complete"
      )

listAllTaskParser :: OPT.Parser HasperCommand
listAllTaskParser =
  HasperListAllTasks
    <$ flag'
      ()
      ( long "all"
          <> short 'a'
          <> help "List all tasks in the repository"
      )

listProgTaskParser :: OPT.Parser HasperCommand
listProgTaskParser =
  HasperListProgTasks
    <$ flag'
      ()
      ( long "progress"
          <> short 'p'
          <> help "List tasks in progress in the repository"
      )

listDoneTaskParser :: OPT.Parser HasperCommand
listDoneTaskParser =
  HasperListDoneTasks
    <$ flag'
      ()
      ( long "progress"
          <> short 'p'
          <> help "List tasks in progress in the repository"
      )

devResetParser :: OPT.Parser HasperCommand
devResetParser =
  HasperDevResetTasks
    <$ flag'
      ()
      ( long "dev-reset"
          <> hidden
      )
