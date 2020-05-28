{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BS.Effects.CdrStore
import           BS.Effects.Crm
import           BS.Effects.InvoiceStore
import           BS.GenerateInvoice
import           BS.Types
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Char8    (unpack)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Function            ((&))
import           Data.IORef
import           Data.IORef
import qualified Data.Map                 as M
import           Data.UUID
import           Data.UUID.V4
import           FM.Fm
import           FM.Free
import           Polysemy
import           Polysemy.State

main :: IO ()
main = execute >>= putStrLn.prettyPrint
  where
    accountId = AccountId 1000
    execute = generateInvoice accountId
      & runCrm
      & runCdrStore
      & runInvoiceStore
      & evalState @CrmMap (M.singleton accountId profile)
      & evalState @CdrMap (M.singleton accountId (cdrs accountId))
      & evalState @InvoiceMap M.empty
      & runM
    prettyPrint = unpack.toStrict.encodePretty

profile :: Profile
profile = Profile "John" "Smith" address plan
  where
    address =
      Address "Backer Street" "221b" "2" "London" "United Kingdom"
    plan = Plan 10 1

cdrs :: AccountId -> [Cdr]
cdrs accountId =
  [ cdr "8abbe08f-4b64-4263-b000-13f3ff77a0c6" Voice 10
  , cdr "bed067b0-3e79-429d-8b96-d1f2c96e79ba" Sms 1
  , cdr "d4bea3d9-a2a7-44cc-8a8d-301051860761" Voice 30
  ]
  where
    cdr uuid callType duration =
      Cdr (maybe nil id (fromString uuid)) accountId callType (Duration duration)
