{-|
Module      : JSON
Description : JSON handling

This module handles all JSON.
JSON is expected to be in the format:
{'type': 'command',
 'commandType': 'press',
 'data': {'keyVal': '<Shift|Ctrl|...|a|b|..|Z>',
          'pressType': '<press|toggle>'}
}

More options to come soon (when/if this is further developed.

As is, it works for basic input.  It hasn't been extensively tested and
almost certainly does not work correctly as documented currently.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JSON
       ( parseMessage
       , Command(..)
       , PressType(..)
       , KeyVal
       ) where

import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Lazy as HML ( lookup )
import Data.Text (unpack)


{-
Some of this JSON parsing could probably have been more automated or clean
than how I did it.  This is something worth looking into/cleaning up if
this project continues.
-}

data KeyVal = Alt | Ctrl | Super | LeftArrow | RightArrow | UpArrow
            | DownArrow | BackSpace | Delete | Shift | Tab | Return
            | Literal String
  deriving (Eq, Generic, Read)

instance Show KeyVal where
  show (Literal s) = s
  show Alt         = "Alt"
  show Ctrl        = "Ctrl"
  show Super       = "Super"
  show LeftArrow   = "Left"
  show RightArrow  = "Right"
  show UpArrow     = "Up"
  show DownArrow   = "Down"
  show BackSpace   = "BackSpace"
  show Delete      = "Delete"
  show Shift       = "Shift"
  show Tab         = "Tab"
  show _           = "Return"

data PressType = Toggle | Press
  deriving (Show, Eq, Generic, Read)
instance FromJSON KeyVal where
  parseJSON v = case v of
    (String "alt")       -> pure Alt
    (String "ctrl")      -> pure Ctrl
    (String "super")     -> pure Super
    (String "left")      -> pure LeftArrow
    (String "right")     -> pure RightArrow
    (String "up")        -> pure UpArrow
    (String "down")      -> pure DownArrow
    (String "backspace") -> pure BackSpace
    (String "delete")    -> pure Delete
    (String "shift")     -> pure Shift
    (String "tab")       -> pure Tab
    (String "return")    -> pure Return
    (String s)           -> pure $ Literal (unpack s)
  parseJSON _ = fail "could not find keyVal object"

instance FromJSON PressType where
  parseJSON (Object v) = case HML.lookup "pressType" v of
    Just (String "press")  -> pure Press 
    Just (String "toggle") -> pure Toggle
    _ -> fail "invalid press type"
  parseJSON (String "press") = pure Press
  parseJSON (String "toggle") = pure Toggle
  parseJSON _ = fail "could not find press type"

data Command = KeyPress
  { keyVal    :: KeyVal
  , pressType :: PressType}
  | MissingData
  | NotCommand
  | IncorrectData
  | UnrecognizedCommand
  deriving (Show, Eq, Generic, Read)

instance FromJSON Command where
  parseJSON (Object v) = case HML.lookup "type" v of
    Just (String "command") -> case HML.lookup "commandType" v of
      Just (String "press")
        -> pressCommand (HML.lookup "data" v) 
      _
        -> pure UnrecognizedCommand
    _                       ->  pure NotCommand
    where pressCommand Nothing = pure MissingData
          pressCommand (Just (Object d)) =  KeyPress <$> d .: "keyVal"
                                                     <*> d .: "pressType"
          pressCommand _ = pure IncorrectData
          
  parseJSON _ = pure NotCommand

parseMessage :: ByteString -> Either String Command
parseMessage = eitherDecode 
