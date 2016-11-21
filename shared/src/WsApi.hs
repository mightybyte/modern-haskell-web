{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module WsApi where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.Text     (Text)
import           GHC.Generics
------------------------------------------------------------------------------

data Up
  = Up_SendChatMessage Text
  deriving (Eq, Show, Generic)

data MessageType = ErrorMsg | InfoMsg
  deriving (Eq, Ord, Show, Generic)

data Down
  = Down_AuthenticatedUser User
  | Down_UserChatMessage User Text
  | Down_Message MessageType Text
  deriving (Eq, Show, Generic)

makePrisms ''Up
makePrisms ''Down

instance FromJSON MessageType
instance ToJSON MessageType
instance FromJSON Up
instance ToJSON Up
instance FromJSON Down
instance ToJSON Down

