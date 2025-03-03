{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import TMCR.IO (readDirectoryFull, Directory (Directory), DirectoryErrorWithContext, readGameDefStrErr, runInMemoryDir, randomSeed)
import TMCR.Logic.Merge (GameDef, defLogic, defShuffles)
import TMCR.Exec (TransactionalShuffleProgress(), initialTShuffleProgress, runAsyncs, runAsyncsLogicOnly, runAsyncsShufflesOnly, stepsAsyncs, tTruthyDescriptors, tCountyDescriptors, tShuffles, tCurrent, tCurrentLatest, tDefinitions, tInQueue, TReadEval(..), tShufflesToForce, tLogicNodes, initializeShuffleQueue, markShufflesDirty)
import System.Exit (exitFailure, exitSuccess)

import qualified Polysemy as P

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M
import qualified Polysemy.Error as P
import qualified Polysemy.Reader as P
import TMCR.Logic.Logic (Scopes (Scopes))
import Control.Monad (when, replicateM, forM, forM_)
import System.Console.Haskeline (InputT, defaultSettings, runInputT, outputStrLn, getInputLine, setComplete, haveTerminalUI, mapInputT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import TMCR.Logic.Descriptor (Descriptor, DescriptorIdent, Oolean, DescriptorType (Truthy, County), SDescriptorType(..))
import TMCR.Logic.Common (Thingy, Nteger)
import TMCR.Shuffler (ShuffleProgress, Consuming, LockIdent, Stateful, ShuffleDependent (LogicNodeDependent), initialShuffleProgress, updateLocal, getDefinitions, defaultEval, Definitions, definedLogicNodes, askAccess)
import TMCR.Logic.Algebra (LogicValue, OolAble)
import Control.Lens
import Control.Lens.TH (makeLenses)
import TMCR.Logic.Graphs (taggedGetNodes)
import TMCR.Logic.Shuffle (RandomSeed, getPartialM)
import Commands
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans ( MonadTrans(lift) )
import System.Random (mkStdGen)
import Control.Monad.State (StateT (runStateT), MonadState (..), gets)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))
import Prelude hiding (log)
import System.IO (stdout, openFile, hPutStrLn, IOMode(..))

import qualified Control.Concurrent.STM.Map as TM

import Control.Concurrent.STM (atomically)

import qualified DeferredFolds.UnfoldlM as DF
import qualified StmContainers.Set as TS
import qualified ListT as LT

data Options = Options {
    module_directory :: String
  , non_interactive :: Bool
  , force_interactive :: Bool
  , option_debug :: Bool
  , option_threadCount :: Int
} deriving (Eq, Ord, Show)

data State = State {
    _stateGameDef :: GameDef
  , _stateOverridesTruthy :: Map (DescriptorIdent 'Truthy, [Thingy]) Oolean
  , _stateOverridesCounty :: Map (DescriptorIdent 'County, [Thingy]) (Nteger, Nteger)
  , _stateProgress :: TransactionalShuffleProgress (LogicValue (Consuming LockIdent (Stateful (OolAble Bool))) 'Truthy) (LogicValue (Consuming LockIdent (Stateful (OolAble Bool))) 'County)
  , _stateSeed :: RandomSeed
  , _stateLoglevel :: LogLevel
}

$(makeLenses 'State)

fromGameDef :: GameDef -> Bool -> IO State
fromGameDef gameDef debug = do
  seed <- randomSeed
  when debug $ putStrLn "Initializing shuffle progress"
  progress <- initialTShuffleProgress gameDef seed
  when debug $ putStrLn "Done Initializing shuffle progress"
  return $ State gameDef M.empty M.empty progress seed $ if debug then LogLevelDebug else LogLevelError

options :: Parser Options
options = Options
  <$> Options.Applicative.argument str (metavar "DIRECTORY")
  <*> switch (long "parse-only" <> short 'p' <> help "Run parser and exit (indicate success)")
  <*> switch (long "force-interactive" <> short 'i' <> help "Use interactive mode even without terminal connected")
  <*> switch (long "debug" <> short 'd' <> help "Start with logging in debug mode (may be changed at any time)")
  <*> option auto (long "threads" <> short 't' <> help "number of threads to run with (default 1)" <> value 1)


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
        interactive <- if (non_interactive options) then return False else if (force_interactive options) then return True else haveTerminalUI
        if interactive then
          liftIO (fromGameDef gameDef (option_debug options)) >>= app options 
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
  cmd <- runReaderT parseCommand (GameContext (state ^. stateDefinitions))
  x <- runStateT (runExceptT $ runCommand options cmd) state
  case x of
    (Left exit, _) -> return $ Left exit
    (Right _, state') -> return $ Right state'

runCommand :: (MonadIO m) => Options -> Command -> ExceptT Bool (StateT State m) ()
runCommand options cmd = case cmd of
    CmdExit -> throwError True
    CmdStep n -> do
      debug $ "Step " <> show n
      progress <- gets $ view stateProgress
      actual_steps <- liftIO $ stepsAsyncs (option_threadCount options) (fromInteger n) defaultEval progress
      debug $ "Actually stepped " <> show actual_steps <> " times"
    CmdRun -> do
      debug "Run"
      progress <- gets $ view stateProgress
      liftIO $ runAsyncs (option_threadCount options) defaultEval progress
    CmdRunLogic -> do
      debug "Run Logic"
      progress <- gets $ view stateProgress
      liftIO $ runAsyncsLogicOnly (option_threadCount options) defaultEval progress
    CmdRunShuffles -> do
      debug "Run Shuffles"
      progress <- gets $ view stateProgress
      liftIO $ runAsyncsShufflesOnly (option_threadCount options) defaultEval progress
    CmdQueueShuffles -> do
      debug "Queue Shuffles"
      progress <- gets $ view stateProgress
      liftIO $ initializeShuffleQueue progress
    CmdDirtyShuffles -> do
      debug "Dirty Shuffles"
      progress <- gets $ view stateProgress
      liftIO $ markShufflesDirty progress
    CmdIsReachable target -> do
      debug "isReachable"
    CmdRandomSeed -> do
      debug "Random Seed"
      seed <- liftIO randomSeed
      runCommand options $ CmdSetSeed seed
    CmdSetSeed seed -> do
      debug "Set Seed"
      gameDef <- gets $ view stateGameDef
      stateSeed .= seed
      progress <- liftIO $ initialTShuffleProgress gameDef seed
      debug "Initialized Progress"
      stateProgress .= progress
    CmdSetLogLevel level -> stateLoglevel .= level
    CmdShowStats -> do
      def <- gets $ view stateGameDef
      let logicNodes = def ^.. defLogic . _1 . to taggedGetNodes . folded . _Just
      debug $ "LogicNodes: " <> show (length logicNodes)
      let shuffles = def ^.. defShuffles . to M.toList . traverse . _1
      debug $ "Shuffles: " <> show (length shuffles)
    CmdShowStatus -> do
      debug "Shuffles"
      shufflesValues <- gets $ view $ stateProgress . tShuffles . tCurrent
      t <- liftIO $ TM.unsafeToList shufflesValues
      traverse (debug . show) t
      debug ""
      debug "Shuffles nextIDs"
      shufflesCount <- gets $ view $ stateProgress . tShuffles . tCurrentLatest
      c <- liftIO $ TM.unsafeToList shufflesCount
      traverse (debug . show) c
      debug ""
      debug "Truthy Descriptors"
      truthyDescriptors <- gets $ view $ stateProgress . tTruthyDescriptors
      a <- liftIO $ TM.unsafeToList truthyDescriptors
      traverse (debug . show) a
      debug ""
      debug "County Descriptors"
      countyDescriptors <- gets $ view $ stateProgress . tCountyDescriptors
      b <- liftIO $ TM.unsafeToList countyDescriptors
      traverse (debug . show) b
      debug ""
      debug "Logic Nodes"
      logicNodes <- gets $ view $ stateProgress . tLogicNodes
      c <- liftIO $ TM.unsafeToList logicNodes
      traverse (debug . show) c
      debug ""
      debug "Accesses"
      accesses <- gets $ view $ stateProgress . tDefinitions . definedLogicNodes . traverse
      currentState <- use stateProgress
      accesses' <- liftIO $ forM accesses $ \(access, args) -> fmap ((,) (access, args)) $ atomically $ flip runReaderT currentState $ runTReadEval $ askAccess STruthy access args
      traverse (debug . show) $ accesses'
      debug ""
    CmdShowQueue -> do
      queue <- gets $ view $ stateProgress . tInQueue
      q <- liftIO $ TM.unsafeToList queue
      traverse (debug . show) q
      debug ""
      shuf <- gets $ view $ stateProgress . tShufflesToForce
      shuf' <- liftIO $ atomically $ LT.toList $ TS.listT shuf
      traverse (debug . show) shuf'
      debug ""
    CmdWriteShuffle s filePath -> do
      h <- case filePath of
        Nothing -> return stdout
        Just filePath -> liftIO $ openFile filePath WriteMode
      currentState <- use stateProgress
      progress <- liftIO $ atomically $ flip runReaderT currentState $ runTReadEval $ getPartialM s
      liftIO $ forM_ progress $ hPutStrLn h . show
      return ()

log :: (MonadIO m) => LogLevel -> String -> ExceptT Bool (StateT State m) ()
log level msg = gets (view stateLoglevel) >>= \level' -> if level' <= level then liftIO $ putStrLn msg else return ()

debug :: (MonadIO m) => String -> ExceptT Bool (StateT State m) ()
debug = log LogLevelDebug

{-
updateOn :: (Monad m) => ShuffleDependent -> ExceptT Bool (StateT State m) ()
updateOn target = do
  defs <- gets $ view stateDefinitions
  progress <- gets $ view stateProgress
  let (newShuffleProgress, addToWorklist) = updateLocal defs defaultEval target progress
  stateProgress .= newShuffleProgress
  stateWorklist <>= addToWorklist
-}

--todo: add overrides
stateDefinitions :: Getter State Definitions
stateDefinitions = stateGameDef . to getDefinitions

loadGameDef options = compile <$> readDirectoryFull (module_directory options)

compile :: Directory -> (Maybe GameDef, Text)
compile dir = either (\x -> (Nothing, x)) (\y -> (Just y, "OK")) $ either (const (Left "Directory Error")) id $ P.run $ P.runError @DirectoryErrorWithContext $ P.runReader @Scopes (Scopes ["area", "room"]) $ P.runError @Text $ runInMemoryDir dir $ readGameDefStrErr (modules dir) where
                modules (Directory m) = M.keys m --todo search for module.yaml
