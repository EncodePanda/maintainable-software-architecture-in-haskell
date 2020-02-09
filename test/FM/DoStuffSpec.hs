{-# LANGUAGE TemplateHaskell #-}
module FM.DoStuffSpec where

import           Data.IORef
import qualified Data.Map              as M
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           FM.Fm                 (Storage (..), doStuff, interpret,
                                        interpretFree)
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range        as Range
import           System.Random         (random)

prop_fetch_add_store_return :: Property
prop_fetch_add_store_return = property $ do
  -- given
  i <- forAll $ Gen.int (Range.constant 0 10)
  uuid <- forAll $ genUUID
  initial <- forAll $ Gen.int (Range.constant 0 10)
  ioRef <- evalIO $ newIORef $ M.singleton uuid initial
  -- when
  res <- evalIO $ interpretFree (interpret ioRef) (doStuff uuid i)
  -- then
  inmem <- evalIO $ readIORef ioRef
  res === "New value: " ++ show (i + initial)
  M.toList inmem === [(uuid, i + initial)]

spec :: IO Bool
spec = checkParallel $$(discover)

genUUID :: Gen UUID
genUUID =
  Gen.generate $ \_ seed ->
  fst . random @UUID $ seed
