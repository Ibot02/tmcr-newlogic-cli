module Commands (Command(..), commandHelp, commandCompletion, parseCommand) where

import System.Console.Haskeline
import System.Console.Haskeline.Completion

import TMCR.Logic.Merge (GameDef)

data Command = CmdExit
             | CmdHelp [String]
            --  | CmdIsReachable (DescriptorIdent 'Truthy, [Thingy])
            --  | CmdStep
            --  | CmdRun

commandHelp :: [String] -> String
commandHelp _ = "todo: help"

parseCommand :: InputT IO Command
parseCommand = do
  i <- fmap words <$> getInputLine "> "
  case i of
    Nothing -> return CmdExit
    Just ("exit":_) -> return CmdExit
    Just ("help":xs) -> return $ CmdHelp xs
    Just _ -> return $ CmdHelp []


commandCompletion :: (Monad m) => GameDef -> CompletionFunc m
commandCompletion _ = noCompletion


-- commands = command "exit" CmdExit noArgs
--          <|> command "isReachable" CmdIsReachable truthyDescriptor