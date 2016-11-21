{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module TemplateApplication.Types.DB where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Data.Time
import           Database.Groundhog
import           Database.Groundhog.TH
import           GroundhogAuth (keyToInt)
------------------------------------------------------------------------------


data Item = Item
    { itemName  :: Text
    , itemCount :: Int
    } deriving (Eq,Show)

mkPersist (defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle })
  [groundhog|
    - entity: Item
  |]
