{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.InvoiceStore where

import           BS.Types       (AccountId, Invoice (..), InvoiceNumber (..))
import qualified Data.Map       as M
import qualified Data.Text      as T
import           Data.UUID      (toText)
import           Data.UUID.V4   (nextRandom)
import           Polysemy
import           Polysemy.State

data InvoiceStore m a where
  StoreInvoice         :: AccountId -> Invoice -> InvoiceStore m ()
  GenNextInvoiceNumber :: AccountId -> InvoiceStore m InvoiceNumber

makeSem ''InvoiceStore

type InvoiceMap = M.Map (AccountId, InvoiceNumber) Invoice

runInvoiceStore ::
     Member (State InvoiceMap) r
  => Member (Embed IO) r
  => Sem (InvoiceStore ': r) a
  -> Sem r a
runInvoiceStore = interpret $ \case
  StoreInvoice accountId invoice ->
    modify (M.insert (accountId, invoiceNumber invoice) invoice)
  GenNextInvoiceNumber accountId ->
    embed $ fmap (InvoiceNumber . toText) nextRandom
