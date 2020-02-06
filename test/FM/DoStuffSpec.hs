{-# LANGUAGE TemplateHaskell #-}
module FM.DoStuffSpec where

import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           FM.Fm                 (Storage (..), doStuff)
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range        as Range
import           System.Random         (random)

prop_returns_plus1 :: Property
prop_returns_plus1 = property $ do
  -- given
  i <- forAll $ Gen.int (Range.constant 0 10)
  uuid <- forAll $ genUUID
  -- when
  let result = doStuff uuid i
  -- then
  let expected = ( Persist uuid (i + 1)
                 , "New value: " ++ (show $ i + 1)
                 )
  result === expected

spec :: IO Bool
spec = checkParallel $$(discover)

genUUID :: Gen UUID
genUUID =
  Gen.generate $ \_ seed ->
  fst . random @UUID $ seed
