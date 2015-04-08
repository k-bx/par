{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Exception.Enclosed (handleAny)
import           Control.Monad              (MonadPlus (..), forever)
import           Data.Foldable
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
             (  fullDesc
             <> progDesc desc
             <> header desc )
    desc = "Run several commands in parallel"

work :: Options -> IO ()
work opts = do
    results <- mapConcurrently runSingle (optCommands opts)
    if optSucceed opts
    then exitWith ExitSuccess
    else maybe (exitWith ExitSuccess)
               (exitWith . NL.head)
               (NL.nonEmpty (filter (/= ExitSuccess) results))

runSingle :: String -> IO ExitCode
runSingle cmd = do
  (_, Just hout, Just herr, ph) <-
      createProcess (shell cmd){ std_out = CreatePipe
                               , std_err = CreatePipe }
  _ <- fork (forwardHandler hout stdout (return :: String -> [String]))
  _ <- (forwardHandler herr stderr (return :: String -> [String]))
  waitForProcess ph

forwardHandler :: (MonadPlus mp, Foldable mp)
               => Handle -> Handle -> (String -> mp String) -> IO ()
forwardHandler from to f = handleAny (const (return ())) $ forever $ do
  l <- hGetLine from
  mapM_ (hPutStr to . (<> "\n")) (f l)
