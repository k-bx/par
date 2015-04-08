{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Enclosed (handleAny)
import           Control.Monad              (MonadPlus (..), forever)
import           Data.Foldable
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
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
    outQ <- newTQueueIO
    errQ <- newTQueueIO
    _ <- fork (runOutqueueFlusher outQ stdout)
    _ <- fork (runOutqueueFlusher errQ stderr)
    results <- mapConcurrently (runSingle outQ errQ) (optCommands opts)
    if optSucceed opts
    then exitWith ExitSuccess
    else maybe (exitWith ExitSuccess)
               (exitWith . NL.head)
               (NL.nonEmpty (filter (/= ExitSuccess) results))

runSingle :: TQueue String -> TQueue String -> String -> IO ExitCode
runSingle outQ errQ cmdBig = do
    (_, Just hout, Just herr, ph) <-
        createProcess (shell cmd){ std_out = CreatePipe
                                 , std_err = CreatePipe }
    _ <- fork (forwardHandler hout outQ (\s -> [cmdPrefix ++ s]))
    _ <- fork (forwardHandler herr errQ (\s -> [cmdPrefix ++ s]))
    waitForProcess ph
  where
    -- TODO: rewrite via Parsec or regex-applicative
    (cmd, cmdPrefix) = if parprefix `L.isPrefixOf` cmdBig
                       then let (pref, rest) = break (== ' ') (drop (length parprefix) cmdBig)
                            in (rest, pref <> " ")
                       else (cmdBig, "")
    parprefix = "PARPREFIX="

forwardHandler :: (MonadPlus mp, Foldable mp)
               => Handle -> TQueue String -> (String -> mp String) -> IO ()
forwardHandler from to f = handleAny (const (return ())) $ forever $ do
    l <- hGetLine from
    mapM_ (\s -> atomically (writeTQueue to (s <> "\n"))) (f l)

runOutqueueFlusher :: TQueue String -> Handle -> IO ()
runOutqueueFlusher queue h = forever (atomically (readTQueue queue)
                                      >>= hPutStr h)
