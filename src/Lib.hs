{-|
Module      : Lib
Description : public functions

This module contains all of the functions that will need to be called to run as an application.
-}

module Lib
    ( runServer
    ) where

import qualified Network.WebSockets as WS
import Control.Monad (forever)
  
import JSON (parseMessage)
import Execute (runCommand)

{-|
top level function that runs the server
-}
runServer :: IO ()
runServer = do
  WS.runServer "127.0.0.1" 5235 application

{-|
the logic of the server
-}
application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  forever $ processMessage conn
  
{-|
the handling of messages
-}
processMessage :: WS.Connection -> IO ()
processMessage conn = do
  msg  <- WS.receiveData conn
  case parseMessage msg of
    Left  x  -> putStrLn   x
    Right x  -> runCommand x
