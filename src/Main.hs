{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Exception.Enclosed (handleAny)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup ((<>))
import Data.String.Class (toStrictByteString)
import Options.Applicative
import Prelude hiding (mapM, mapM_)
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
      results <-
        waitingPipeHandlers
          (runOutqueueFlusher outQ stdout numCmds)
          (runOutqueueFlusher errQ stderr numCmds)
          (mapConcurrently (runSingle debug outQ errQ) (optCommands opts))
      let cmdAndRes = zip (optCommands opts) results
      case filter ((/= ExitSuccess) . snd) cmdAndRes of
        [] -> exitSuccess
        (c,r):_ -> do
          hPutStrLn stderr $ "Failed command:\n" <> c
          exitWith r
    Just masterProcNum -> do
      outQMain <- newTBQueueIO 1024
      errQMain <- newTBQueueIO 1024
      withAsync (runOutqueueFlusher outQ stdout numCmds) $ \_ ->
        withAsync (runOutqueueFlusher errQ stderr numCmds) $ \_ -> do
          let (xs, m:ys) = splitAt masterProcNum (optCommands opts)
              (master, rest) = (m, xs ++ ys)
          mapM_ (async . runSingle debug outQ errQ) rest
          status <- waitingPipeHandlers (forwardWaiting outQMain outQ)
            (forwardWaiting errQMain errQ) $
            runSingle debug outQMain errQMain master
          exitWith status
  where
    forwardWaiting from to = go
      where
        go = do
          v <- atomically (readTBQueue from)
          when (isJust v) (atomically (writeTBQueue to v) >> go)

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
  s <- waitingPipeHandlers
    (forwardPrefixing hout outQ)
    (forwardPrefixing herr errQ)
    (waitForProcess ph)
  debug $ "Process " <> show cmdBig <> " exited with status " <> show s
  return s
  where
    -- TODO: rewrite via Parsec or regex-applicative
    (cmd, cmdPrefix) =
      if parprefix `L.isPrefixOf` cmdBig
        then let (pref, rest) = break (== ' ') (drop (length parprefix) cmdBig)
             in (rest, pref <> " ")
        else (cmdBig, "")
    parprefix = "PARPREFIX="
    toBs = toStrictByteString
    prefixer chunk = [toBs cmdPrefix <> chunk]
    forwardPrefixing from to = forwardHandler from to prefixer

waitingPipeHandlers :: IO a -> IO b -> IO c -> IO c
waitingPipeHandlers outH errH inner =
  withAsync outH $ \out ->
    withAsync errH $ \err -> do
      res <- inner
      void $ waitBoth out err
      return res

forwardHandler ::
     Handle
  -> TBQueue (Maybe ByteString)
  -> (ByteString -> [ByteString])
  -> IO ()
forwardHandler from to f = fin (hndl go)
  where
    go = do
      eof <- hIsEOF from
      unless eof $ do
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
