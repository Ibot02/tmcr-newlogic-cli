{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import TMCR.IO (readDirectoryFull, Directory (Directory), DirectoryErrorWithContext, readGameDefStrErr, runInMemoryDir, randomSeed)
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
import Control.Monad (when, replicateM)
import System.Console.Haskeline (InputT, defaultSettings, runInputT, outputStrLn, getInputLine, setComplete, haveTerminalUI, mapInputT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import TMCR.Logic.Descriptor (Descriptor, DescriptorIdent, Oolean, DescriptorType (Truthy, County))
import TMCR.Logic.Common (Thingy, Nteger)
import TMCR.Shuffler (ShuffleProgress, Consuming, LockIdent, Stateful, ShuffleDependent (LogicNodeDependent), initialShuffleProgress, updateLocal, getDefinitions, defaultEval, Definitions)
import TMCR.Logic.Algebra (LogicValue, OolAble)
import Control.Lens
import Control.Lens.TH (makeLenses)
import TMCR.Logic.Graphs (taggedGetNodes)
import TMCR.Logic.Shuffle (RandomSeed)
import Commands
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans ( MonadTrans(lift) )
import System.Random (mkStdGen)
import Control.Monad.State (StateT (runStateT), MonadState (..), gets)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))
import Prelude hiding (log)


data Options = Options {
    module_directory :: String
  , non_interactive :: Bool
} deriving (Eq, Ord, Show)

data State = State {
    _stateGameDef :: GameDef
  , _stateOverridesTruthy :: Map (DescriptorIdent 'Truthy, [Thingy]) Oolean
  , _stateOverridesCounty :: Map (DescriptorIdent 'County, [Thingy]) (Nteger, Nteger)
  , _stateProgress :: ShuffleProgress (LogicValue (Consuming LockIdent (Stateful (OolAble Bool))) 'Truthy) (LogicValue (Consuming LockIdent (Stateful (OolAble Bool))) 'County)
  , _stateWorklist :: [ShuffleDependent]
  , _stateSeed :: RandomSeed
  , _stateLoglevel :: LogLevel
}

$(makeLenses 'State)

fromGameDef :: GameDef -> IO State
fromGameDef gameDef = do
  seed <- randomSeed
  return $ State gameDef M.empty M.empty (initialShuffleProgress gameDef seed) (initialWorklist gameDef) seed LogLevelDebug

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
          liftIO (fromGameDef gameDef) >>= app options 
        else return True
      if success then exitSuccess else exitFailure

app :: Options -> State -> InputT IO Bool
app options s = do
  retcode <- repl options s
  case retcode of
    Right s' -> app options s'
    Left e -> return e

repl :: Options -> State -> InputT IO (Either Bool State)
repl _options state = do
  cmd <- runReaderT parseCommand GameContext
  x <- runStateT (runExceptT $ runCommand cmd) state
  case x of
    (Left exit, _) -> return $ Left exit
    (Right _, state') -> return $ Right state'

runCommand :: (MonadIO m) => Command -> ExceptT Bool (StateT State m) ()
runCommand cmd = case cmd of
    CmdExit -> throwError True
    CmdStep n -> do
      debug $ "Step " <> show n
      step n
    CmdRun -> do
      debug "Run"
      runCommand $ CmdStep 10000
    CmdIsReachable target -> do
      debug "isReachable"
    CmdRandomSeed -> do
      debug "Random Seed"
      seed <- liftIO randomSeed
      runCommand $ CmdSetSeed seed
    CmdSetSeed seed -> do
      debug "Set Seed"
      gameDef <- gets $ view stateGameDef
      stateSeed .= seed
      stateProgress .= initialShuffleProgress gameDef seed
      stateWorklist .= initialWorklist gameDef
    CmdSetLogLevel level -> stateLoglevel .= level

log :: (MonadIO m) => LogLevel -> String -> ExceptT Bool (StateT State m) ()
log level msg = gets (view stateLoglevel) >>= \level' -> if level' <= level then liftIO $ putStrLn msg else return ()

debug :: (MonadIO m) => String -> ExceptT Bool (StateT State m) ()
debug = log LogLevelDebug

step :: (MonadIO m) => Integer -> ExceptT Bool (StateT State m) ()
step 0 = return ()
step n | n < 0 = return ()
       | otherwise = do
        first <- popWorklist
        case first of
          Nothing -> return ()
          Just target -> do
            debug $ "Stepping on: " <> show target
            updateOn target
            step (n - 1)
        return ()

updateOn :: (Monad m) => ShuffleDependent -> ExceptT Bool (StateT State m) ()
updateOn target = do
  defs <- gets $ view stateDefinitions
  progress <- gets $ view stateProgress
  let (newShuffleProgress, addToWorklist) = updateLocal defs defaultEval target progress
  stateProgress .= newShuffleProgress
  stateWorklist <>= addToWorklist

--todo: add overrides
stateDefinitions :: Getter State Definitions
stateDefinitions = stateGameDef . to getDefinitions

popWorklist :: (Monad m) => ExceptT Bool (StateT State m) (Maybe ShuffleDependent)
popWorklist = zoom stateWorklist $ state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, filter (/= x) xs)


loadGameDef options = compile <$> readDirectoryFull (module_directory options)

compile :: Directory -> (Maybe GameDef, Text)
compile dir = either (\x -> (Nothing, x)) (\y -> (Just y, "OK")) $ either (const (Left "Directory Error")) id $ P.run $ P.runError @DirectoryErrorWithContext $ P.runReader @Scopes (Scopes ["area", "room"]) $ P.runError @Text $ runInMemoryDir dir $ readGameDefStrErr (modules dir) where
                modules (Directory m) = M.keys m --todo search for module.yaml
