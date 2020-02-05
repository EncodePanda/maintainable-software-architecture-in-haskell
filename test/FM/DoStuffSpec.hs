{-# LANGUAGE TemplateHaskell #-}
module FM.DoStuffSpec where

import           FM.Fm          (doStuff)
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

prop_returns_plus1 :: Property
prop_returns_plus1 = property $ do
  -- given
  i <- forAll $ Gen.int (Range.constant 0 10)
  -- when
  let res = doStuff i
  -- then
  res === "New value: " ++ (show $ i + 1)

spec :: IO Bool
spec = checkParallel $$(discover)
