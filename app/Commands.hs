{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
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
import TMCR.Logic.Shuffle (RandomSeed, ShuffleName)
import TMCR.Shuffler (Definitions, definedShuffles)
import System.Random (mkStdGen)
import Text.Read (readMaybe)

import Data.List (isPrefixOf)

import qualified Data.Map as M

import System.IO (FilePath)
import Data.Char (isSpace)

import Control.Lens

import qualified Data.Text as T

data Command = CmdExit
             | CmdIsReachable (DescriptorIdent 'Truthy, [Thingy])
             | CmdStep Integer
             | CmdRun
             | CmdRunLogic
             | CmdAutoRunLogic
             | CmdRunShuffles
             | CmdQueueShuffles
             | CmdDirtyShuffles
             | CmdRandomSeed
             | CmdSetSeed RandomSeed
             | CmdSetLogLevel LogLevel
             | CmdShowStats
             | CmdShowStatus
             | CmdShowQueue
             | CmdWriteShuffle ShuffleName (Maybe FilePath)

data GameContext = GameContext {
    _gameContextDefinitions :: Definitions
  } deriving (Eq, Ord, Show)

data LogLevel = LogLevelDebug | LogLevelError deriving (Eq, Ord, Show)

$(makeLenses ''GameContext)

parseCommand :: ReaderT GameContext (InputT IO) Command
parseCommand = do
  i <- fmap words <$> lift (getInputLine "> ")
  x <- parseCommand' definedCommands CmdExit i
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
  fmap ($ t) $ parseCommands'' cs xs
parseCommand'' (CmdSpecConstant _ _) [] = lift $ Left []
parseCommand'' (CmdSpecConstant c cs) (x:xs) | c == x = parseCommands'' cs xs
                                             | otherwise = lift $ Left []
parseCommand'' (CmdSpecNumber cs) [] = lift $ Left []
parseCommand'' (CmdSpecNumber cs) (x:xs) = case readMaybe x of
  Just n -> fmap ($ n) $ parseCommands'' cs xs
  Nothing -> lift $ Left ["Failed to parse number"]
parseCommand'' (CmdSpecShuffle cs) [] = lift $ Left ["missing <SHUFFLE>"]
parseCommand'' (CmdSpecShuffle cs) (x:xs) = do
  s <- findShuffle x
  fmap ($ s) $ parseCommands'' cs xs
parseCommand'' (CmdSpecFile _) [] = lift $ Left ["missing <FILENAME>"]
parseCommand'' (CmdSpecFile cs) (x:xs) = do
  fmap ($ x) $ parseCommands'' cs xs

findTarget :: String -> ReaderT GameContext (Either [String]) (DescriptorIdent 'Truthy, [Thingy])
findTarget _ = lift $ Left ["Target finding: To be implemented"]

findShuffle :: String -> ReaderT GameContext (Either [String]) ShuffleName
findShuffle name = do
  let name' = T.pack name
  s <- asks $ view $ gameContextDefinitions . definedShuffles . at name'
  case s of
    Just _ -> return name'
    Nothing -> lift $ Left ["Shuffle " <> name <> " does not exist"]

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
getHelp'' helpText (CmdSpecShuffle xs) = fmap (first ("<SHUFFLE>":)) (getHelp' helpText xs)
getHelp'' helpText (CmdSpecFile xs) = fmap (first ("<FILENAME>":)) (getHelp' helpText xs)

commandCompletion :: (MonadIO m) => GameContext -> CompletionFunc m
commandCompletion def = completeWordWithPrev' Nothing isSpace (completeCommands def definedCommands . words . reverse)

completeCommands :: (MonadIO m) => GameContext -> CommandTree a -> [String] -> String -> m [Completion]
completeCommands def commands words part = fmap concat $ forM commands $ \command -> completeCommand def command words part

completeCommand :: (MonadIO m) => GameContext -> CommandSpec a -> [String] -> String -> m [Completion]
completeCommand def (CmdSpecHelptext _ cmd) words part = completeCommand def cmd words part
completeCommand def (CmdSpecConstant c _) [] part | part `isPrefixOf` c = return [simpleCompletion c]
                                                  | otherwise = return []
completeCommand def (CmdSpecConstant c cmds) (c':cs) p | c == c' = completeCommands def cmds cs p
                                                       | otherwise = return []
completeCommand def (CmdSpecLeaf _) _ _ = return []
completeCommand def (CmdSpecNumber _) [] _ = return []
completeCommand def (CmdSpecNumber cmds) (c:cs) p = case readMaybe @Integer c of
  Nothing -> return []
  Just _ -> completeCommands def cmds cs p
completeCommand def (CmdSpecTarget cmds) [] p = return [] --todo
completeCommand def (CmdSpecTarget cmds) (_:cs) p = completeCommands def cmds cs p
completeCommand def (CmdSpecShuffle cmds) [] p = return $ def ^.. gameContextDefinitions . definedShuffles . to M.keys . traverse . to T.unpack . filtered (p `isPrefixOf`) . to simpleCompletion
completeCommand def (CmdSpecShuffle cmds) (_:cs) p = completeCommands def cmds cs p
completeCommand def (CmdSpecFile cmds) [] p = listFiles p
completeCommand def (CmdSpecFile cmds) (_:cs) p = completeCommands def cmds cs p
completeCommand def _ _ _ = return []

type CommandTree a = [CommandSpec a]

data CommandSpec a =
    CmdSpecConstant String (CommandTree a)
  | CmdSpecLeaf a
  | CmdSpecNumber (CommandTree (Integer -> a))
  | CmdSpecTarget (CommandTree ((DescriptorIdent 'Truthy, [Thingy]) -> a))
  | CmdSpecShuffle (CommandTree (ShuffleName -> a))
  | CmdSpecFile (CommandTree (FilePath -> a))
  | CmdSpecHelptext String (CommandSpec a)
  deriving (Functor)


definedCommands :: CommandTree Command
definedCommands = [ CmdSpecHelptext "exit the program" $ CmdSpecConstant "exit" $ pure $ CmdSpecLeaf CmdExit
           , CmdSpecHelptext "Test whether the given target is reachable" $ CmdSpecConstant "isReachable" [CmdSpecTarget [CmdSpecLeaf CmdIsReachable]]
           , CmdSpecConstant "step" $ [ CmdSpecHelptext "step the shuffler once" $ CmdSpecLeaf (CmdStep 1)
                                      , CmdSpecHelptext "step the shuffler a number of times" $ CmdSpecNumber $ pure $ CmdSpecLeaf CmdStep
                                      ]
           , CmdSpecHelptext "step the shuffler until it is done" $ CmdSpecConstant "run" $ pure $ CmdSpecLeaf CmdRun
           , CmdSpecHelptext "step the shuffler until it is done with logic" $ CmdSpecConstant "runLogic" $ pure $ CmdSpecLeaf CmdRunLogic
           , CmdSpecHelptext "automatically step the shuffler until it is done with logic" $ CmdSpecConstant "autoRunLogic" $ pure $ CmdSpecLeaf CmdAutoRunLogic
           , CmdSpecHelptext "step the shuffler until it is done with shuffles" $ CmdSpecConstant "runShuffles" $ pure $ CmdSpecLeaf CmdRunShuffles
           , CmdSpecHelptext "queue shuffles based on logic eval" $ CmdSpecConstant "queueShuffles" $ pure $ CmdSpecLeaf CmdQueueShuffles
           , CmdSpecHelptext "queue logic eval assuming all shuffles changed" $ CmdSpecConstant "dirtyShuffles" $ pure $ CmdSpecLeaf CmdDirtyShuffles
           , CmdSpecHelptext "randomize the seed and reset shuffle progress" $ CmdSpecConstant "randomSeed" $ pure $ CmdSpecLeaf CmdRandomSeed
           , CmdSpecHelptext "set the seed and reset shuffle progress" $ CmdSpecConstant "seed" $ pure $ CmdSpecNumber $ pure $ CmdSpecLeaf $ CmdSetSeed . mkStdGen . fromInteger
           , CmdSpecHelptext "show statistics about the game definitions" $ CmdSpecConstant "stats" $ pure $ CmdSpecLeaf $ CmdShowStats
           , CmdSpecHelptext "show status of the shuffle" $ CmdSpecConstant "status" $ pure $ CmdSpecLeaf $ CmdShowStatus
           , CmdSpecHelptext "show the current shuffler queue" $ CmdSpecConstant "queue" $ pure $ CmdSpecLeaf $ CmdShowQueue
           , CmdSpecHelptext "writes a shuffle, to stdout or the given file" $ CmdSpecConstant "dumpShuffle" $ pure $ CmdSpecShuffle $ fmap (flip CmdWriteShuffle) <$>
                             [ CmdSpecLeaf $ Nothing
                             , CmdSpecFile $ pure $ CmdSpecLeaf Just
                             ]
           , CmdSpecHelptext "set the log level" $ CmdSpecConstant "logs" $ fmap CmdSetLogLevel <$>
                [ CmdSpecConstant "debug" $ pure $ CmdSpecLeaf $ LogLevelDebug
                , CmdSpecConstant "error" $ pure $ CmdSpecLeaf $ LogLevelError
                ]
           ]
