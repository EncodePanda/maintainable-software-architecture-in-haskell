{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.InvoiceStore where

import           BS.Types (AccountId, Invoice, InvoiceNumber)
import           Polysemy

data InvoiceStore m a where
  StoreInvoice         :: AccountId -> Invoice -> InvoiceStore m ()
  GenNextInvoiceNumber :: AccountId -> InvoiceStore m InvoiceNumber

makeSem ''InvoiceStore
