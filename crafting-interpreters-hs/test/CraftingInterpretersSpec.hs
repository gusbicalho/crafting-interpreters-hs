{-# LANGUAGE BlockArguments #-}
module CraftingInterpretersSpec where

import Test.Hspec
import CraftingInterpreters

spec :: Spec
spec = do
  describe "test" do
    it "works" $
      (1 :: Int) `shouldBe` 1
