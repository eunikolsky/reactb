{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.QQ
import Lib (rmEmptyArrays)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rmEmptyArrays" $ do
    it "removes empty arrays" $ do
      let (Object obj) = [aesonQQ| {"empty": [], "foo": {"bar": {"nil": [], "rest": 42}}, "nested": {"array": []}} |]
          (Object expected) = [aesonQQ| {"foo": {"bar": {"rest": 42}}, "nested": {}} |]
      rmEmptyArrays obj `shouldBe` expected
