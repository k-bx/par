{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Enclosed    (handleAny)
import           Control.Monad                 (MonadPlus (..), forever)
import           Data.Foldable
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NL
import           Options.Applicative
import           Prelude                       hiding (mapM, mapM_)
import           SlaveThread                   (fork)
import           System.Exit
import           System.IO
import           System.Process
import           Text.InterpolatedString.Perl6

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
    desc = [q|
Run several commands in parallel
================================

`par` is a small utility that runs multiple commands in parallel and
by default exits with a failure status of a first failure it sees.

Use `--help` for command-line help.

Basic usage example
-------------------

    > par "echo foo; sleep 1; echo foo; sleep 1; echo foo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
    foo
    bar
    bar
    foo
    bar
    foo
    success
    > par "echo foo; sleep 1; foofoo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
    bar
    foo
    bar
    /bin/sh: foofoo: command not found
    bar

Adding prefix to output
-----------------------

    > par "PARPREFIX=[fooechoer] echo foo" "echo bar"
    [fooechoer] foo
    bar

Force success exit-code
-----------------------

    > par --succeed "foo" "bar" && echo 'wow'
    /bin/sh: foo: command not found
    /bin/sh: bar: command not found
    wow

Installation
------------

For Ubuntu 12.04, 14.04 and MacOS X download some release and put it
into $PATH. For others -- see "building from source" instructions.

https://github.com/k-bx/par/releases

Example:

```
cd /tmp
wget https://github.com/k-bx/par/releases/download/1.0.0/par-ubuntu-12.04.tar.gz
tar xvf ./par-ubuntu-12.04.tar.gz
sudo mv ./par /usr/local/bin/
```

Building from source
--------------------

1. [Install Haskell's GHC compiler](http://www.stackage.org/install)
2. run `make`. Produced executable will be inside `./dist/build/par/par`
3. optionally, run `sudo make install` to copy into `/usr/local/bin/`

Please note that `par` uses specific set of versions to build, setting
them via cabal.config file. If you want to remove these constraints,
just remove the file.

Footnote on strings in bash/zsh
-------------------------------

Many people know that strings in bash and zsh are "weird", but not
many people know that there are good old ASCII-strings also present.

Double-quoted strings are interpolating variables and do other interesting
things like reacting on "!" sign, for example.

Single-quotes don't interpolate variables and don't react on "!" sign, but
they also don't let you quote neither single-quote nor double-quote.

Turns out good old ASCII-quotes are available as $'string' syntax! Example:

> echo $'foo'
foo
> echo $'foo with "doublequotes and \'singletuoes\' inside"!'
foo with "doublequotes and 'singletuoes' inside"!

You are a better person with this knowledge now. $'Enjoy!'
|]

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
    parprefix = "PARPREFIX="::String

forwardHandler :: (MonadPlus mp, Foldable mp)
               => Handle -> TQueue String -> (String -> mp String) -> IO ()
forwardHandler from to f = handleAny (const (return ())) $ forever $ do
    l <- hGetLine from
    mapM_ (\s -> atomically (writeTQueue to (s <> "\n"))) (f l)

runOutqueueFlusher :: TQueue String -> Handle -> IO ()
runOutqueueFlusher queue h = forever (atomically (readTQueue queue)
                                      >>= hPutStr h)
