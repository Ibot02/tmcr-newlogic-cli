{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Options.Applicative
import TMCR.IO (readDirectoryFull, Directory (Directory), DirectoryErrorWithContext, readGameDefStrErr, runInMemoryDir)
import TMCR.Logic.Merge (GameDef, defLogic)
import System.Exit (exitFailure, exitSuccess)

import qualified Polysemy as P

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M
import qualified Polysemy.Error as P
import qualified Polysemy.Reader as P
import TMCR.Logic.Logic (Scopes (Scopes))
import Control.Monad (when)
import System.Console.Haskeline (InputT, defaultSettings, runInputT, outputStrLn, getInputLine, setComplete, haveTerminalUI)
import Control.Monad.IO.Class (liftIO)
import TMCR.Logic.Descriptor (Descriptor, DescriptorIdent, Oolean, DescriptorType (Truthy, County))
import TMCR.Logic.Common (Thingy, Nteger)
import TMCR.Shuffler (ShuffleProgress, Consuming, LockIdent, Stateful, ShuffleDependent (LogicNodeDependent), initialShuffleProgress)
import TMCR.Logic.Algebra (LogicValue, OolAble)
import Control.Lens
import TMCR.Logic.Graphs (taggedGetNodes)
import TMCR.Logic.Shuffle (RandomSeed)
import Commands


data Options = Options {
    module_directory :: String
  , non_interactive :: Bool
} deriving (Eq, Ord, Show)

data State = State {
    _stateGameDef :: GameDef
  , _stateOverridesTruthy :: Map (DescriptorIdent 'Truthy, [Thingy]) Oolean
  , _stateOverridesCounty :: Map (DescriptorIdent 'County, [Thingy]) (Nteger, Nteger)
  , _stateProgress :: Maybe (ShuffleProgress (LogicValue (Consuming LockIdent (Stateful (OolAble Bool))) 'Truthy) (LogicValue (Consuming LockIdent (Stateful (OolAble Bool))) 'County))
  , _stateWorklist :: [ShuffleDependent]
  , _stateSeed :: Maybe RandomSeed
}


fromGameDef :: GameDef -> State
fromGameDef gameDef = State gameDef M.empty M.empty Nothing (initialWorklist gameDef) Nothing where
  initialWorklist :: GameDef -> [ShuffleDependent]
  initialWorklist def = let nodes = def ^.. defLogic . _1 . to taggedGetNodes . folded . _Just
                          --edges = foldMap (\n -> def ^.. defLogic . _1 . to (taggedGetEdgesTo (Just n)) . folded . _2 . to getDisjunctions . folded . folded) nodes
                       in {-fmap DescriptorDependend edges <> -} fmap LogicNodeDependent nodes


options :: Parser Options
options = Options
  <$> Options.Applicative.argument str (metavar "DIRECTORY")
  <*> switch (long "parse-only" <> short 'p' <> help "Run parser and exit (indicate success)")


main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper) (fullDesc <> progDesc "Parse and interact with TMCRs new logic format")
  runApp opts


runApp :: Options -> IO ()
runApp options = do
  gameDef <- loadGameDef options
  case gameDef of
    (Nothing, err) -> putStrLn (T.unpack err) >> exitFailure
    (Just gameDef, _) -> do
      success <- runInputT (setComplete (commandCompletion gameDef) defaultSettings) $ do
        interactive <- if (non_interactive options) then return False else haveTerminalUI
        if interactive then
          app options (fromGameDef gameDef)
        else return True
      if success then exitSuccess else exitFailure

app :: Options -> State -> InputT IO Bool
app options s = do
  retcode <- repl options s
  case retcode of
    Right s' -> app options s'
    Left e -> return e

repl :: Options -> State -> InputT IO (Either Bool State)
repl options state = do
  cmd <- parseCommand
  case cmd of
    CmdExit -> return $ Left True
    CmdHelp topic -> outputStrLn (commandHelp topic) >> return (Right state)

loadGameDef options = compile <$> readDirectoryFull (module_directory options)

compile :: Directory -> (Maybe GameDef, Text)
compile dir = either (\x -> (Nothing, x)) (\y -> (Just y, "OK")) $ either (const (Left "Directory Error")) id $ P.run $ P.runError @DirectoryErrorWithContext $ P.runReader @Scopes (Scopes ["area", "room"]) $ P.runError @Text $ runInMemoryDir dir $ readGameDefStrErr (modules dir) where
                modules (Directory m) = M.keys m --todo search for module.yaml
