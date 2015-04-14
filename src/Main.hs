{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Concurrent         (ThreadId)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception          (finally)
import           Control.Exception.Enclosed (handleAny)
import           Control.Monad              (MonadPlus (..))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Foldable
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
import           Data.String.Class          (toStrictByteString)
import           Options.Applicative
import           Prelude                    hiding (mapM, mapM_)
import           SlaveThread                (fork)
import           System.Exit
import           System.IO
import           System.Process

data Options = Options
    { optSucceed  :: Bool
    , optCommands :: [String] }
    deriving (Eq, Show)

parser :: Parser Options
parser = Options
  <$> flag False True
      ( long "succeed"
     <> help "Return 0 code no matter what" )
  <*> some (argument str (metavar "COMMANDS..."))

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
    if optSucceed opts
    then exitWith ExitSuccess
    else maybe (exitWith ExitSuccess)
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
    (_, w1) <- forkW (forwardHandler hout outQ (\s -> [toBs cmdPrefix <> s]))
    (_, w2) <- forkW (forwardHandler herr errQ (\s -> [toBs cmdPrefix <> s]))
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

forwardHandler :: (MonadPlus mp, Foldable mp)
               => Handle -> TBQueue (Maybe ByteString) -> (ByteString -> mp ByteString) -> IO ()
forwardHandler from to f = fin (hndl go)
  where
    go = do
      eof <- hIsEOF from
      if eof then return ()
      else do
        l <- B.hGetSome from (1024 * 1024)
        mapM_ (\s -> atomically (writeTBQueue to (Just s))) (f l)
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
