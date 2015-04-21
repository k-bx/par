{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent         (ThreadId)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception          (finally)
import           Control.Exception.Enclosed (handleAny)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BSC8
import           Data.Foldable
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
import           Data.String.Class          (toStrictByteString)
import           Options.Applicative
import           Prelude                    hiding (mapM, mapM_)
import           Safe
import           SlaveThread                (fork)
import           System.Exit
import           System.IO
import           System.Process

data Options = Options
    { optCommands :: [String] }
    deriving (Eq, Show)

parser :: Parser Options
parser = Options
  <$> some (argument str (metavar "COMMANDS..."))

main :: IO ()
main = execParser opts >>= work
  where
    opts = info (helper <*> parser)
             (fullDesc
              <> progDesc desc)
    desc = "Run several commands in parallel"

work :: Options -> IO ()
work opts = do
    outQ <- newTBQueueIO 1024
    errQ <- newTBQueueIO 1024
    let numCmds = length (optCommands opts)
    (_, w1) <- forkW (runOutqueueFlusher outQ stdout numCmds)
    (_, w2) <- forkW (runOutqueueFlusher errQ stderr numCmds)
    results <- mapConcurrently (runSingle outQ errQ) (optCommands opts)
    waitSignal w1 >> waitSignal w2
    maybe (exitWith ExitSuccess)
          (exitWith . NL.head)
          (NL.nonEmpty (filter (/= ExitSuccess) results))

newtype WaitSignal = WaitSignal (MVar Bool)

waitSignal :: WaitSignal -> IO ()
waitSignal (WaitSignal mv) = takeMVar mv >> return ()

-- | Fork with a wait-signal ability
forkW :: IO a -> IO (ThreadId, WaitSignal)
forkW f = do
    ws <- newEmptyMVar
    tid <- fork (finally f (putMVar ws True))
    return (tid, WaitSignal ws)

runSingle :: TBQueue (Maybe ByteString) -> TBQueue (Maybe ByteString) -> String -> IO ExitCode
runSingle outQ errQ cmdBig = do
    (_, Just hout, Just herr, ph) <-
        createProcess (shell cmd){ std_out = CreatePipe
                                 , std_err = CreatePipe }
    (_, w1) <- forkW (forwardHandler hout outQ prefixer)
    (_, w2) <- forkW (forwardHandler herr errQ prefixer)
    res <- waitForProcess ph
    waitSignal w1 >> waitSignal w2
    return res
  where
    -- TODO: rewrite via Parsec or regex-applicative
    (cmd, cmdPrefix) = if parprefix `L.isPrefixOf` cmdBig
                       then let (pref, rest) = break (== ' ') (drop (length parprefix) cmdBig)
                            in (rest, pref <> " ")
                       else (cmdBig, "")
    parprefix = "PARPREFIX="
    toBs = toStrictByteString
    prefixer chunk = [toBs cmdPrefix <> chunk]

forwardHandler :: Handle
               -> TBQueue (Maybe ByteString)
               -> (ByteString -> [ByteString])
               -> IO ()
forwardHandler from to f = fin (hndl go)
  where
    go = do
      eof <- hIsEOF from
      if eof then return ()
      else do
        line <- B.hGetLine from
        atomically (writeTBQueue to (Just (B.concat (map (<> "\n") (f line)))))
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
        Nothing -> go (n-1)
        Just l -> B.hPut h l >> go n
