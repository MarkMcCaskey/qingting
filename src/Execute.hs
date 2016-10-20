{-# LANGUAGE OverloadedStrings #-}

module Execute
  ( runCommand )where

import JSON (Command, KeyVal)

import System.Process hiding (runCommand)
import JSON

runCommand :: Command -> IO ()
runCommand c = print c >> pressKey c
           
           
{-|
Takes a 'Command' and does the associated key press using xdotool.
      
There needs to be explicit handling of external programs (maybe as coeffects?)
      
additionally, this function is incomplete
-}           
pressKey :: Command -> IO ()
pressKey kp = case pressType kp of
                Press  -> createProcess (proc "xdotool" ["key", show (keyVal kp)]) >> return ()
                Toggle -> undefined
