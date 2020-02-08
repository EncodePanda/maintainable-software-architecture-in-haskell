{-# LANGUAGE TemplateHaskell #-}
module FM.DoStuffSpec where

import           Data.IORef
import qualified Data.Map              as M
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           FM.Fm                 (Storage (..), doStuff, interpret)
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
  let expected = (Persist uuid (i+1) (
                     Persist uuid (i+1) (
                         Done $ "New value: " ++ (show $ i + 1)
                 )))
  result === expected

prop_persists_once :: Property
prop_persists_once = property $ do
  -- given
  i <- forAll $ Gen.int (Range.constant 0 10)
  uuid <- forAll $ genUUID
  ioRef <- evalIO $ newIORef M.empty
  -- when
  let res = doStuff uuid i
  evalIO $ interpret ioRef res
  -- then
  inmem <- evalIO $ readIORef ioRef
  M.toList inmem === [(uuid, i + 1)]

spec :: IO Bool
spec = checkParallel $$(discover)

genUUID :: Gen UUID
genUUID =
  Gen.generate $ \_ seed ->
  fst . random @UUID $ seed
