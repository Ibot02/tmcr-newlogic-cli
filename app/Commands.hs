{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Commands (Command(..), GameContext(..), commandCompletion, parseCommand, LogLevel(..)) where

import System.Console.Haskeline
import System.Console.Haskeline.Completion

import TMCR.Logic.Merge (GameDef)
import TMCR.Logic.Common (Thingy)
import TMCR.Logic.Descriptor (DescriptorName, DescriptorType (Truthy), DescriptorIdent)

import Control.Monad.Reader (MonadReader(..), ReaderT (runReaderT), asks)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad
import Control.Arrow (Arrow(first))
import TMCR.Logic.Shuffle (RandomSeed)
import System.Random (mkStdGen)
import Text.Read (readMaybe)

data Command = CmdExit
             | CmdIsReachable (DescriptorIdent 'Truthy, [Thingy])
             | CmdStep Integer
             | CmdRun
             | CmdRandomSeed
             | CmdSetSeed RandomSeed
             | CmdSetLogLevel LogLevel

data GameContext = GameContext

data LogLevel = LogLevelDebug | LogLevelError deriving (Eq, Ord, Show)

commandHelp :: [String] -> String
commandHelp _ = "todo: help"

parseCommand :: ReaderT GameContext (InputT IO) Command
parseCommand = do
  i <- fmap words <$> lift (getInputLine "> ")
  x <- parseCommand' commands CmdExit i
  maybe parseCommand return x

parseCommand' :: CommandTree a -> a -> Maybe [String] -> ReaderT GameContext (InputT IO) (Maybe a)
parseCommand' commands onEOF = \case
    Nothing -> pure $ Just onEOF
    Just ("help":_) -> printHelp commands >> return Nothing
    Just xs -> do
      r <- asks (either (const Nothing) Just . runReaderT (parseCommands'' commands xs))
      when (null r) $ printHelp commands
      return r

parseCommands'' :: CommandTree a -> [String] -> ReaderT GameContext (Either [String]) a
parseCommands'' commands input = do
  context <- ask
  lift $ foldr (firstRightWith (<>)) (Left []) $ fmap (`runReaderT` context) $ fmap (`parseCommand''` input) commands

firstRightWith :: (a0 -> a0 -> a0) -> Either a0 a -> Either a0 a -> Either a0 a
firstRightWith _ (Right a) _ = Right a
firstRightWith _ _ (Right a) = Right a
firstRightWith combine (Left x) (Left y) = Left (x `combine` y)

parseCommand'' :: CommandSpec a -> [String] -> ReaderT GameContext (Either [String]) a
parseCommand'' (CmdSpecHelptext _ x) xs = parseCommand'' x xs
parseCommand'' (CmdSpecLeaf a) [] = return a
parseCommand'' (CmdSpecLeaf _) _ = lift $ Left ["unexpected params"]
parseCommand'' (CmdSpecTarget _) [] = lift $ Left ["Missing <TARGET>"]
parseCommand'' (CmdSpecTarget cs) (x:xs) = do
  t <- findTarget x
  fmap ($t) $ parseCommands'' cs xs
parseCommand'' (CmdSpecConstant _ _) [] = lift $ Left []
parseCommand'' (CmdSpecConstant c cs) (x:xs) | c == x = parseCommands'' cs xs
                                             | otherwise = lift $ Left []
parseCommand'' (CmdSpecNumber cs) [] = lift $ Left []
parseCommand'' (CmdSpecNumber cs) (x:xs) = case readMaybe x of
  Just n -> fmap ($n) $ parseCommands'' cs xs
  Nothing -> lift $ Left ["Failed to parse number"]

findTarget :: String -> ReaderT GameContext (Either [String]) (DescriptorIdent 'Truthy, [Thingy])
findTarget _ = lift $ Left ["Target finding: To be implemented"]

printHelp :: (MonadIO m) => CommandTree a -> m ()
printHelp xs = void $ liftIO $ traverse putStrLn $ getHelp xs

getHelp :: CommandTree a -> [String]
getHelp = fmap (\(cmdParts, helpText) -> unwords cmdParts <> " - " <> helpText) . getHelp' "???"

getHelp' :: String -> CommandTree a -> [([String], String)]
getHelp' helpText tree = tree >>= getHelp'' helpText

getHelp'' :: String -> CommandSpec a -> [([String], String)]
getHelp'' helpText (CmdSpecConstant x xs) = fmap (first (x:)) (getHelp' helpText xs)
getHelp'' helpText (CmdSpecLeaf _) = [([], helpText)]
getHelp'' helpText (CmdSpecTarget xs) = fmap (first ("<TARGET>":)) (getHelp' helpText xs)
getHelp'' _ (CmdSpecHelptext helpText x) = getHelp'' helpText x
getHelp'' helpText (CmdSpecNumber xs) = fmap (first ("<NUMBER>":)) (getHelp' helpText xs)

commandCompletion :: (Monad m) => GameDef -> CompletionFunc m
commandCompletion _ = noCompletion

type CommandTree a = [CommandSpec a]

data CommandSpec a =
    CmdSpecConstant String (CommandTree a)
  | CmdSpecLeaf a
  | CmdSpecNumber (CommandTree (Integer -> a))
  | CmdSpecTarget (CommandTree ((DescriptorIdent 'Truthy, [Thingy]) -> a))
  | CmdSpecHelptext String (CommandSpec a)


commands :: CommandTree Command
commands = [ CmdSpecHelptext "exit the program" $ CmdSpecConstant "exit" $ pure $ CmdSpecLeaf CmdExit
           , CmdSpecHelptext "Test whether the given target is reachable" $ CmdSpecConstant "isReachable" [CmdSpecTarget [CmdSpecLeaf CmdIsReachable]]
           , CmdSpecConstant "step" $ [ CmdSpecHelptext "step the shuffler once" $ CmdSpecLeaf (CmdStep 1)
                                      , CmdSpecHelptext "step the shuffler a number of times" $ CmdSpecNumber $ pure $ CmdSpecLeaf CmdStep
                                      ]
           , CmdSpecHelptext "step the shuffler until it is done" $ CmdSpecConstant "run" $ pure $ CmdSpecLeaf CmdRun
           , CmdSpecHelptext "randomize the seed and reset shuffle progress" $ CmdSpecConstant "randomSeed" $ pure $ CmdSpecLeaf CmdRandomSeed
           , CmdSpecHelptext "set the seed and reset shuffle progress" $ CmdSpecConstant "seed" $ pure $ CmdSpecNumber $ pure $ CmdSpecLeaf $ CmdSetSeed . mkStdGen . fromInteger
           , CmdSpecHelptext "set the log level" $ CmdSpecConstant "logs" $ [ CmdSpecConstant "debug" $ pure $ CmdSpecLeaf $ CmdSetLogLevel LogLevelDebug
                                                                            , CmdSpecConstant "error" $ pure $ CmdSpecLeaf $ CmdSetLogLevel LogLevelError
                                                                            ]
           ]