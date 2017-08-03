{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Exception.Enclosed (handleAny)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.List as L
import qualified Data.List.NonEmpty as NL
import Data.Maybe
import Data.Semigroup ((<>))
import Data.String.Class (toStrictByteString)
import Options.Applicative
import Prelude hiding (mapM, mapM_)
import SlaveThread (fork, forkFinally)
import System.Exit
import System.IO
import System.Process

data Options = Options
  { optMasterProcess :: Maybe Int
  , optVerbose :: Bool
  , optCommands :: [String]
  } deriving (Eq, Show)

parser :: Parser Options
parser =
  Options
  -- TODO: instead of "read" use something better
   <$>
  (fmap (fmap read) . optional . strOption $
   long "master-process" <> metavar "MASTER_PROCESS" <>
   help
     "Master process index, starting from 0. Indicates command, which lifetime and exit-code only matter")
  <*>
  option auto
  ( long "verbose"
    <> help "Print debug output"
    <> showDefault
    <> value False
    <> metavar "BOOL" )
  <*>
  some (argument str (metavar "COMMANDS..."))

main :: IO ()
main = execParser opts >>= work
  where
    opts = info (helper <*> parser) (fullDesc <> progDesc desc)
    desc = "Run several commands in parallel"

work :: Options -> IO ()
work opts = do
  let debug msg = when (optVerbose opts) $ putStrLn msg
  outQ <- newTBQueueIO 1024
  errQ <- newTBQueueIO 1024
  let numCmds = length (optCommands opts)
  case optMasterProcess opts of
    Nothing -> do
      (_, w1) <- forkW (runOutqueueFlusher outQ stdout numCmds)
      (_, w2) <- forkW (runOutqueueFlusher errQ stderr numCmds)
      results <- mapConcurrently (runSingle debug outQ errQ) (optCommands opts)
      let cmdAndRes = zip (optCommands opts) results
      waitSignal w1 >> waitSignal w2
      maybe
        (exitWith ExitSuccess)
        (\rs -> do
           let (c, r) = NL.head rs
           hPutStrLn stderr ("Failed command:\n" <> c)
           exitWith r)
        (NL.nonEmpty (filter ((/= ExitSuccess) . snd) cmdAndRes))
    Just masterProcNum -> do
      outQMain <- newTBQueueIO 1024
      errQMain <- newTBQueueIO 1024
      _ <- fork (runOutqueueFlusher outQ stdout numCmds)
      _ <- fork (runOutqueueFlusher errQ stderr numCmds)
      let (xs, (m:ys)) = splitAt masterProcNum (optCommands opts)
          (master, rest) = (m, xs ++ ys)
      mapM_ (fork . runSingle debug outQ errQ) rest
      (_, w1) <- forkW (forwardWaiting outQMain outQ)
      (_, w2) <- forkW (forwardWaiting errQMain errQ)
      status <- runSingle debug outQMain errQMain master
      debug $ "Master process " <> show m <> " exited with status " <> show status
      waitSignal w1 >> waitSignal w2
      debug $ "Pipes drained, exiting"
      exitWith status
  where
    forwardWaiting from to = go
      where
        go = do
          v <- atomically (readTBQueue from)
          when (isJust v) (atomically (writeTBQueue to v) >> go)

newtype WaitSignal =
  WaitSignal (MVar Bool)

waitSignal :: WaitSignal -> IO ()
waitSignal (WaitSignal mv) = takeMVar mv >> return ()

-- | Fork with a wait-signal ability
forkW :: IO a -> IO (ThreadId, WaitSignal)
forkW f = do
  ws <- newEmptyMVar
  tid <- forkFinally (putMVar ws True) f
  return (tid, WaitSignal ws)

runSingle ::
     (String -> IO ())
  -> TBQueue (Maybe ByteString)
  -> TBQueue (Maybe ByteString)
  -> String
  -> IO ExitCode
runSingle debug outQ errQ cmdBig = do
  debug $ "Starting process " <> show cmd <> ", output prefix " <> show cmdPrefix 
  (_, Just hout, Just herr, ph) <-
    createProcess (shell cmd) {std_out = CreatePipe, std_err = CreatePipe}
  (_, w1) <- forkW (forwardHandler hout outQ prefixer)
  (_, w2) <- forkW (forwardHandler herr errQ prefixer)
  res <- waitForProcess ph
  waitSignal w1 >> waitSignal w2
  return res
    -- TODO: rewrite via Parsec or regex-applicative
  where
    (cmd, cmdPrefix) =
      if parprefix `L.isPrefixOf` cmdBig
        then let (pref, rest) = break (== ' ') (drop (length parprefix) cmdBig)
             in (rest, pref <> " ")
        else (cmdBig, "")
    parprefix = "PARPREFIX="
    toBs = toStrictByteString
    prefixer chunk = [toBs cmdPrefix <> chunk]

forwardHandler ::
     Handle
  -> TBQueue (Maybe ByteString)
  -> (ByteString -> [ByteString])
  -> IO ()
forwardHandler from to f = fin (hndl go)
  where
    go = do
      eof <- hIsEOF from
      if eof
        then return ()
        else do
          line <- B.hGetLine from
          atomically
            (writeTBQueue to (Just (B.concat (map (<> "\n") (f line)))))
          go
    hndl = handleAny (const (return ()))
    fin f' = finally f' (atomically (writeTBQueue to Nothing))

runOutqueueFlusher :: TBQueue (Maybe ByteString) -> Handle -> Int -> IO ()
runOutqueueFlusher queue h numCmds = go numCmds
  where
    go 0 = return ()
    go n = do
      ml <- atomically (readTBQueue queue)
      case ml of
        Nothing -> go (n - 1)
        Just l -> B.hPut h l >> go n
