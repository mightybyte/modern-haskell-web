{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import           Data.List
import qualified Data.Map as M
import           Test.Hspec
------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "foo" $ do
    it "has a property" $ do
      2 * 5 `shouldBe` 10
