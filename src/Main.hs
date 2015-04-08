{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Options.Applicative
import Data.Text

data Options = Options
    { optAlwaysSuccess :: Bool
    , optCommands :: [String] }
    deriving (Eq, Show)

options :: Parser Options
options = Options
      <$> switch
          ( long "always-success"
         <> help "Return 0 code no matter what" )
      <*> some (argument str (metavar "COMMANDS..."))

main :: IO ()
main = do
  putStrLn "Hi!"
